-- File:        top.vhd
-- Author:      Richard James Howe
-- Repository:  https://github.com/howerj/subleq-vhdl
-- Email:       howe.r.j.89@gmail.com
-- License:     MIT
-- Description: Top level entity; SUBLEQ CPU
--
-- This module brings together the SUBLEQ CPU/Memory subsystem with
-- the I/O, which is a UART.

library ieee, work, std;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.all;
use work.uart_pkg.all;

entity top is
	generic (
		g:               common_generics := default_settings;
		file_name:       string          := "subleq.dec";
		N:               positive        := 16;
		baud:            positive        := 115200;
		debug:           natural         := 0; -- will not synthesize if greater than zero (debug off = 0)
		uart_use_cfg:    boolean         := false;
		uart_fifo_depth: natural         := 0
	);
	port (
		clk:         in std_ulogic;
		-- synthesis translate_off
--		rst:         in std_ulogic;
		halted:     out std_ulogic;
		blocked:    out std_ulogic;
		-- synthesis translate_on
		tx:         out std_ulogic;
		rx:          in std_ulogic);
end entity;

architecture rtl of top is
	constant data_length:  positive := N;
	constant W:            positive := N - 3;
	constant addr_length:  positive := W;
	constant clks_per_bit: integer  := calc_clks_per_bit(g.clock_frequency, baud);
	constant delay:        time     := g.delay;

	signal rst: std_ulogic := '0';
	signal bsy, hav, io_re, io_we: std_ulogic := 'U';
	signal obyte, ibyte: std_ulogic_vector(7 downto 0) := (others => 'U');

	signal c_hav, n_hav: std_ulogic := '0';
	signal c_ibyte, n_ibyte: std_ulogic_vector(7 downto 0) := (others => '0');

begin
	assert not (io_re = '1' and io_we = '1') severity warning;

	process (clk, rst) begin -- N.B. We could use register components for this
		if rst = '1' and g.asynchronous_reset then
			c_hav <= '0';
			c_ibyte <= (others => '0');
		elsif rising_edge(clk) then
			c_hav <= n_hav after delay;
			c_ibyte <= n_ibyte after delay;
			if rst = '1' and not g.asynchronous_reset then
				c_hav <= '0';
				c_ibyte <= (others => '0');			
			end if;
		end if;
	end process;

	process (c_hav, c_ibyte, hav, ibyte, io_re) begin
		n_hav <= c_hav after delay;
		n_ibyte <= c_ibyte after delay;

		if hav = '1' then
			n_hav <= '1' after delay;
			n_ibyte <= ibyte after delay;
		end if;

		if io_re = '1' then
			n_hav <= '0' after delay;
		end if;
	end process;

	system: entity work.system
	generic map(
		g => g,
		file_name => file_name,
		N => N,
		debug => debug)
	port map (
		clk     => clk,
		rst     => rst,
		-- synthesis translate_off
		halted  => halted,
		blocked => blocked,
		-- synthesis translate_on
		obyte   => obyte,
		ibyte   => c_ibyte,
		obsy    => bsy,
		ihav    => c_hav,
		io_we   => io_we, 
		io_re   => io_re);

	uart_tx_0: entity work.uart_tx
		generic map(clks_per_bit => clks_per_bit, delay => delay)
		port map(
			clk => clk,
			tx_we => io_we,
			tx_byte => obyte,
			tx_active => bsy,
			tx_serial => tx,
			tx_done => open);

	uart_rx_0: entity work.uart_rx
		generic map(clks_per_bit => clks_per_bit, delay => delay)
		port map(
			clk => clk,
			rx_serial => rx,
			rx_have_data => hav,
			rx_byte => ibyte);

end architecture;


