-- File:        top.vhd
-- Author:      Richard James Howe
-- Repository:  https://github.com/howerj/subleq-vhdl
-- License:     MIT
-- Description: Top level entity; SUBLEQ CPU

library ieee, work, std;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.all;

entity top is
	generic (
		g:               common_generics := default_settings;
		file_name:       string          := "subleq.hex";
		N:               positive        := 16;
		baud:            positive        := 115200;
		debug:           natural         := 0; -- will not synthesize if greater than zero (debug off)
		uart_use_cfg:    boolean         := false;
		uart_fifo_depth: natural         := 0
	);
	port (
		clk:         in std_ulogic;
		-- synthesis translate_off
--		rst:         in std_ulogic;
		halt:       out std_ulogic;
		-- synthesis translate_on
		tx:         out std_ulogic;
		rx:          in std_ulogic;
		sw:          in std_ulogic_vector(7 downto 0);
		ld:         out std_ulogic_vector(7 downto 0));
end entity;

architecture rtl of top is
	constant W:        positive   := N - 1;
	signal rst:        std_ulogic := '0';
	signal i, o, a:    std_ulogic_vector(N - 1 downto 0) := (others => '0');
	signal oe, ie, ae: std_ulogic := 'X';
begin
	peripheral: entity work.peripherals
		generic map(
			g               => g,
			file_name       => file_name,
			W               => W,
			N               => N,
			baud            => baud,
			uart_fifo_depth => uart_fifo_depth,
			uart_use_cfg    => uart_use_cfg)
		port map (
			clk => clk, rst => rst,
			tx => tx, rx => rx, ld => ld, sw => sw,
			i => o,
			o => i,
			a => a, oe => ie, ie => oe, ae => ae);

	cpu: entity work.subleq
		generic map (
			asynchronous_reset => g.asynchronous_reset,
			delay              => g.delay,
			N                  => N,
			debug              => debug)
		port map (
			clk => clk, rst => rst,
			-- synthesis translate_off
			stop => halt,
			-- synthesis translate_on
			i => i,
			o => o, a => a, oe => oe, ie => ie, ae => ae);
end architecture;
