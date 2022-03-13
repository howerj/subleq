-- File:        subleq.vhd
-- Author:      Richard James Howe
-- Repository:  https://github.com/howerj/subleq-vhdl
-- License:     MIT
-- Description: 16-bit SUBLEQ machine

library ieee, work, std;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all; -- for debug only, not needed for synthesis

entity subleq is
	generic (
		asynchronous_reset: boolean    := true;   -- use asynchronous reset if true, synchronous if false
		delay:              time       := 0 ns;   -- simulation only, gate delay
		N:                  positive   := 16;     -- size the CPU
		debug:              natural    := 0);     -- debug level, 0 = off
	port (
		clk, rst:       in std_ulogic;
		i:              in std_ulogic_vector(N - 1 downto 0);
		o, a:          out std_ulogic_vector(N - 1 downto 0);
		oe, ie, ae: buffer std_ulogic;
		stop:          out std_ulogic);
end;

architecture rtl of subleq is
	type state_t is (RESET, FETCH_A, LOAD_A, FETCH_B, LOAD_B, SUBTRACT, STORE_B, FETCH_C, JUMP, HALT);

	type registers_t is record
		state: state_t;
		pc: std_ulogic_vector(N - 1 downto 0);
		ra, rla, rb, rlb, rc: std_ulogic_vector(N - 1 downto 0);
	end record;

	constant registers_default: registers_t := (
		state  => RESET,
		pc     => (others => '0'),
		ra     => (others => '0'),
		rla    => (others => '0'),
		rb     => (others => '0'),
		rlb    => (others => '0'),
		rc     => (others => '0')
	);
		
	signal c, f: registers_t := registers_default;

	-- Obviously this does not synthesis, which is why synthesis is turned
	-- off for the body of this function, it does make debugging much easier
	-- though, we will be able to see which instructions are executed and do so
	-- by name.
	procedure print_debug_info is
		variable ll: line;

		function hx(slv: in std_ulogic_vector) return string is -- std_ulogic_vector to hex string
			constant cv: string := "0123456789ABCDEF";
			constant qu: integer := slv'length   / 4;
			constant rm: integer := slv'length mod 4;
			variable rs: string(1 to qu);
			variable sl: std_ulogic_vector(3 downto 0);
		begin
			assert rm = 0 severity failure;
			for l in 0 to qu - 1 loop
				sl := slv((l * 4) + 3 downto (l * 4));
				rs(qu - l) := cv(to_integer(unsigned(sl)) + 1);
			end loop;
			return rs;
		end function;

		function yn(sl: std_ulogic; ch: character) return string is -- print a flag
			variable rs: string(1 to 2) := "- ";
		begin
			if sl = '1' then
				rs(1) := ch;
			end if;
			return rs;
		end function;
	begin
		-- synthesis translate_off
		if debug > 0 then
			if c.state = FETCH_C then
				write(ll, hx(c.pc)    & ": ");
				--write(ll, cmd_t'image(cmd)   & HT);
				write(ll, hx(c.ra)   & " ");
				write(ll, hx(c.rla)  & " ");
				write(ll, hx(c.rb)   & " ");
				write(ll, hx(c.rlb)  & " ");
				write(ll, hx(c.rc)   & " ");
				--write(ll, yn(c.flags(HLT), 'H'));
				writeline(OUTPUT, ll);
			end if;
		--	if debug > 1 and last = '1' then
		--		write(ll, state_t'image(c.state) & " => ");
		--		write(ll, state_t'image(f.state));
		--		writeline(OUTPUT, ll);
		--	end if;
		end if;
		-- synthesis translate_on
	end procedure;

	--signal special_address: std_ulogic := 'X';
begin
	assert N >= 8                      report "CPU Width too small: N >= 8"    severity failure;
	assert not (ie = '1' and oe = '1') report "input/output at the same time"  severity failure;
	assert not (ie = '1' and ae = '1') report "input whilst changing address"  severity failure;
	assert not (oe = '1' and ae = '1') report "output whilst changing address" severity failure;

	--special_address <= '1' when c.ra = (others => '1') and c.rb = (others => '1') else '0';

	process (clk, rst) begin
		if rst = '1' and asynchronous_reset then
			c.state <= RESET after delay;
		elsif rising_edge(clk) then
			c <= f after delay;
			if rst = '1' and not asynchronous_reset then
				c.state <= RESET after delay;
			else
				-- These are just assertions/debug logging, they are not required for
				-- running, but we can make sure there are no unexpected state transitions,
				-- and report on the internal state.
				print_debug_info;
				if c.state = RESET  then assert f.state = FETCH_A;   end if;
				if c.state = HALT then assert f.state = HALT; end if;
			end if;
		end if;
	end process;

	process (i, c) begin
		f <= c after delay;
		o <= (others => '0') after delay;
		oe <= '0' after delay;
		ie <= '0' after delay;
		ae <= '0' after delay;
		stop <= '0' after delay;

		case c.state is
		when RESET =>
			f <= registers_default;
			f.state <= FETCH_A;
		when FETCH_A =>
			a <= c.pc;
			ae <= '1';
			f.ra <= c.pc;
			f.state <= LOAD_A;
		when LOAD_A => 
			ie <= '1';
			f.rla <= i;
			f.pc <= std_ulogic_vector(unsigned(c.pc) + 1);
			f.state <= FETCH_B;
		when FETCH_B =>
			a <= c.pc;
			ae <= '1';
			f.rb <= c.pc;
			f.state <= LOAD_B;
		when LOAD_B => 
			ie <= '1';
			f.rlb <= i;
			f.pc <= std_ulogic_vector(unsigned(c.pc) + 1);
			f.state <= SUBTRACT;
		when SUBTRACT =>
			f.rlb <= std_ulogic_vector(unsigned(c.rlb) - unsigned(c.rla));
			f.state <= STORE_B;
		when STORE_B =>
			if c.ra(c.ra'high) = '0' and c.rb(c.rb'high) = '0' then
				oe <= '1';
				o <= c.rlb;
			end if;
			f.pc <= std_ulogic_vector(unsigned(c.pc) + 1);
			a <= c.pc;
			ae <= '1';
			f.state <= FETCH_C;
		when FETCH_C =>
			ie <= '1';
			f.rc <= i;
			f.state <= JUMP;
		when JUMP => 
			if c.ra(c.ra'high) = '0' and c.rb(c.rb'high) = '0' then -- no jump on special instructions
			end if;
			f.state <= FETCH_A;
		when HALT =>
			stop <= '1' after delay;
		end case;
	end process;

end architecture;

