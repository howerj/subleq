-- File:        subleq.vhd
-- Author:      Richard James Howe
-- Repository:  https://github.com/howerj/subleq-vhdl
-- License:     MIT / Public Domain
-- Description: SUBLEQ CPU
--
-- This is a SUBLEQ CPU, or a One Instruction Set Computer (OISC), it
-- is Turing complete and capable of running a Forth interpreter (with
-- the appropriate image). See <https://github.com/howerj/subleq> for
-- the source of the eForth image, another one of my projects.
--
-- A version that uses both ports of a Dual Port Block RAM would have
-- fewer states and would operate faster (the operands `a` and `b`
-- could be loaded in one cycle, then `mem[a]` and `mem[b]` the next
-- cycle instead of the four cycles it takes in this implementation).
--
-- There are advantages to this single port design however. It means
-- that if a Dual Port Block RAM was to host the SUBLEQ program then
-- this CPU would only use one of the I/O channels for it, the other
-- channel could be used for other circuitry allowing it to communicate
-- with the SUBLEQ CPU and program. Input and Output is quite the
-- weakness in this design, only a single byte of input and output
-- is allowed which is really only suitable for a UART.
--
-- It would be nice to make as much configurable as possible (via
-- generics) to make the design more flexible. This has been done to
-- an extent where it is easy (for example you can invert the jump
-- condition if you want and input can be made to be non-blocking).
--
-- Extra things that could be done:
--
-- * Turn this file into a design with multiple components that can
-- just be placed down, including default SUBLEQ programs (such as
-- the mentioned Forth interpreter). This would mean moving the
-- Block RAM component into here, and the UART would need simplifying.
-- * Improve Input/Output capabilities allowing multiple peripherals
-- to be hung off of the design. This can be done if `strict_io` is
-- disabled, the high bit indicates I/O, the lower bits would indicate
-- the I/O address.
-- * The design document <https://www.gaisler.com/doc/vhdl2proc.pdf>,
-- or "Fault-tolerant Microprocessors for Space Applications" by
-- Jiri Gaisler describes a two process model for structuring components
-- in VHDL that is partially followed here. This could be improved with
-- record types for the modules input and output signals (except the
-- clock and reset lines). For this to work the same methodology needs
-- to be applied to the whole project.
--
--
library ieee, work, std;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all; -- Used for debug only (turned off for synthesis)

entity subleq is
	generic (
		asynchronous_reset: boolean    := true;   -- use asynchronous reset if true, synchronous if false
		delay:              time       := 0 ns;   -- simulation only, gate delay
		N:                  positive   := 16;     -- size the CPU
		jspec:              std_ulogic_vector(4 downto 0) := "11111"; -- Jump specification
		non_blocking_input: boolean    := false;  -- if true, input will be -1 if there is no input
		strict_io:          boolean    := true;   -- if true, I/O happens when `a` or `b` are -1, else when high bit set
		debug:              natural    := 0);     -- debug level, 0 = off
	port (
		clk:           in std_ulogic; -- Guess what this is?
		rst:           in std_ulogic; -- Can be sync or async
		o:            out std_ulogic_vector(N - 1 downto 0); -- Memory access; Output
		i:             in std_ulogic_vector(N - 1 downto 0); -- Memory access; Input
		a:            out std_ulogic_vector(N - 1 downto 0); -- Memory access; Address
		we, re:       out std_ulogic; -- Write and read enable for memory only
		obyte:        out std_ulogic_vector(7 downto 0); -- UART output byte
		ibyte:         in std_ulogic_vector(7 downto 0); -- UART input byte
		obsy, ihav:    in std_ulogic; -- Output busy / Have input
		io_we, io_re: out std_ulogic; -- Write and read enable for I/O (UART)
		pause:         in std_ulogic; -- pause the CPU in the `S_A` state
		blocked:      out std_ulogic; -- is the CPU paused, or blocking on I/O?
		halted:       out std_ulogic); -- Is the system halted?
end;

architecture rtl of subleq is
	type state_t is (
		S_RESET,  -- Starting State
		S_A,      -- Load A / Check if input
		S_B,      -- Load B / Check if output
		S_LA,     -- Load m[A] / Jump to output
		S_LB,     -- Load m[B] / Jump to input
		S_STORE,  -- Store Result
		S_JMP,    -- PC = C
		S_NJMP,   -- No Jump
		S_IN,     -- Wait for input
		S_OUT,    -- Output byte when ready
		S_HALT);  -- Stop for it is the time of hammers

	type registers_t is record
		b:      std_ulogic_vector(N - 1 downto 0); -- Multi purpose register
		la:     std_ulogic_vector(N - 1 downto 0); -- Multi purpose register
		pc:     std_ulogic_vector(N - 1 downto 0); -- Program Counter
		state:  state_t;    -- CPU State Register
		stop:   std_ulogic; -- CPU Halt Flag
		input:  std_ulogic; -- CPU Instruction-is-input Flag
		output: std_ulogic; -- CPU Instruction-is-output Flag
	end record;

	constant registers_default: registers_t := (
		b      => (others => '0'),
		la     => (others => '0'),
		pc     => (others => '0'),
		state  => S_RESET,
		stop   => '0',
		input  => '0',
		output => '0');

	signal c, f: registers_t := registers_default; -- All state is captured in here
	signal jump, zero, ones, high, io, dop: std_ulogic := '0'; -- Transient CPU Flags
	signal sub, npc: std_ulogic_vector(N - 1 downto 0) := (others => '0');

	constant AZ: std_ulogic_vector(N - 1 downto 0) := (others => '0'); -- All Zeros
	constant AO: std_ulogic_vector(N - 1 downto 0) := (others => '1'); -- All Ones

	-- These constants are used to index into the jump specification, this allows
	-- us to make alternative OISC machines by specifying a generic instead of making
	-- an entirely new machine.
	constant JS_C:   integer := 0; -- Jump on all condition or'd = '1' or '0'
	constant JS_ZEN: integer := 1; -- Enable Jumping on zero comparison 
	constant JS_ZC:  integer := 2; -- '1' = Jump on Zero, '0' = Jump on Non-Zero if enabled
	constant JS_NEN: integer := 3; -- Enable Jumping on negative (high bit set)
	constant JS_NC:  integer := 4; -- '1' = Jump on Negative, '0' = Jump on 

	-- Obviously this does not synthesize, which is why synthesis is turned
	-- off for the body of this function, it does make debugging much easier
	-- though, we will be able to see which instructions are executed and do so
	-- by name.
	procedure print_debug_info is
		variable oline: line;
		function int(slv: in std_ulogic_vector) return string is
		begin
			return integer'image(to_integer(signed(slv)));
		end function;
	begin
		-- synthesis translate_off
		if debug >= 2 then
			write(oline, int(c.pc)  & ": ");
			write(oline, state_t'image(c.state) & HT);
			write(oline, int(c.b)   & " ");
			write(oline, int(c.la)  & " ");
			if debug >= 3 and c.state /= f.state then
				write(oline, state_t'image(c.state) & " => ");
				write(oline, state_t'image(f.state));
			end if;
			writeline(OUTPUT, oline);
		end if;
		-- synthesis translate_on
	end procedure;
begin
	-- The following asserts could be placed in this module if what
	-- they were asserting was "buffered". As they are not, they go
	-- in the next module up.
	--
	--   assert not (re = '1' and we = '1') severity warning;
	--   assert not (io_re = '1' and io_we = '1') severity warning;

	assert not (c.input = '1' and c.output = '1') report "Input and Output flag set a the same time" severity warning;
	assert N >= 8 report "SUBLEQ machine width too small, must be greater or equal to 8 bits" severity failure;

	npc   <= std_ulogic_vector(unsigned(c.pc) + 1) after delay;
	sub   <= std_ulogic_vector(unsigned(i) - unsigned(c.la)) after delay;
	zero  <= '1' when jspec(JS_ZEN) = '1' and c.la = AZ else '0' after delay;
	jump  <= '1' when (jspec(JS_NEN) = '1' and c.la(c.la'high) = jspec(JS_NC)) or zero = jspec(JS_ZC) else '0' after delay;
	o     <= c.la after delay;
	obyte <= c.la(obyte'range) after delay;
	ones  <= '1' when strict_io and i = AO else '0' after delay;
	high  <= '1' when (not strict_io) and i(i'high) = '1' else '0' after delay;
	io    <= '1' when ones = '1' or high = '1' else '0' after delay;
	re    <= not dop after delay;
	we    <= dop after delay;

	process (clk, rst) begin
		if rst = '1' and asynchronous_reset then
			c.state <= S_RESET after delay;
		elsif rising_edge(clk) then
			c <= f after delay;
			if rst = '1' and not asynchronous_reset then
				c.state <= S_RESET after delay;
			else
				print_debug_info;

				if c.state = S_RESET then assert f.state = S_A severity warning; end if;
				if c.state = S_HALT  then assert f.state = S_HALT severity warning; end if;
				if c.state = S_A     then assert f.state = S_B or f.state = S_A or f.state = S_HALT severity warning; end if;
				if c.state = S_B     then assert f.state = S_LA or f.state = S_IN severity warning; end if;
				if c.state = S_LA    then assert f.state = S_LB or f.state = S_OUT severity warning; end if;
				if c.state = S_LB    then assert f.state = S_STORE severity warning; end if;
				if c.state = S_STORE then assert f.state = S_JMP or f.state = S_NJMP severity warning; end if;
				if c.state = S_JMP   then assert f.state = S_A severity warning; end if;
				if c.state = S_NJMP  then assert f.state = S_A severity warning; end if;
				if c.state = S_IN    then assert f.state = S_IN or f.state = S_STORE severity warning; end if;
				if c.state = S_OUT   then assert f.state = S_OUT or f.state = S_A severity warning; end if;
			end if;
		end if;
	end process;

	process (c, i, npc, io, jump, sub, ibyte, obsy, ihav, pause) begin
		f <= c after delay;
		halted <= '0' after delay;
		io_we <= '0' after delay;
		io_re <= '0' after delay;
		dop <= '0' after delay; -- read enabled when `dop='0'`, write otherwise
		a <= c.pc after delay;
		blocked <= '0' after delay;
		if c.pc(c.pc'high) = '1' then f.stop <= '1' after delay; end if;

		case c.state is
		when S_RESET => 
			f <= registers_default after delay;
			f.state <= S_A after delay;
			a <= (others => '0') after delay;
		when S_A =>
			f.state <= S_B after delay;
			f.la <= i after delay;
			a <= npc after delay;
			f.pc <= npc after delay;
			f.input <= '0' after delay;
			f.output <= '0' after delay;

			if io = '1' then
				f.input <= '1' after delay;
			end if;

			if c.stop = '1' then
				f.state <= S_HALT after delay;
			elsif pause = '1' then
				blocked <= '1' after delay;
				f.state <= S_A after delay;
			end if;
		when S_B =>
			f.state <= S_LA after delay;
			f.b <= i after delay;
			a <= c.la after delay;
			f.pc <= npc after delay;
			if io = '1' then
				f.output <= '1' after delay;
			end if;

			if c.input = '1' then -- skip S_LA
				a <= i after delay;
				f.state <= S_IN after delay;
				f.la <= (others => '0') after delay;
				f.la(ibyte'range) <= ibyte after delay;
			end if;
		when S_LA =>
			f.state <= S_LB after delay;
			f.la <= i after delay;
			a <= c.b after delay;
			if c.output = '1' then
				a <= c.pc after delay;
				f.state <= S_OUT after delay;
			end if;
		when S_LB =>
			f.state <= S_STORE after delay;
			f.la <= sub after delay;
			a <= c.pc after delay;
		when S_STORE =>
			f.state <= S_NJMP after delay;
			f.b <= i after delay;
			f.pc <= npc after delay;
			a <= c.b after delay;
			dop <= '1' after delay;
			if jump = jspec(JS_C) and c.input = '0' then
				f.state <= S_JMP after delay;
			end if;
		when S_JMP =>
			f.state <= S_A after delay;
			a <= c.b after delay;
			f.pc <= c.b after delay;
		when S_NJMP => -- This performs a read using `c.pc`
			f.state <= S_A after delay;
		when S_IN =>
			a <= c.b after delay;
			f.la <= (others => '0') after delay;
			f.la(ibyte'range) <= ibyte after delay;
			blocked <= '1' after delay;
			if ihav = '1' then
				f.state <= S_STORE after delay;
				io_re <= '1' after delay;
				blocked <= '0' after delay;
			elsif non_blocking_input then
				f.state <= S_STORE after delay;
				f.la <= (others => '1') after delay;
				blocked <= '0' after delay;
			end if;
		when S_OUT =>
			a <= npc after delay;
			f.pc <= npc after delay;
			blocked <= '1' after delay;
			if obsy = '0' then
				f.state <= S_A after delay;
				io_we <= '1' after delay;
				blocked <= '0' after delay;
			end if;
		when S_HALT =>
			halted <= '1' after delay;
		end case;
	end process;
end architecture;
