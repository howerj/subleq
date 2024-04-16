-- PACKAGE: UART TX/RX
-- AUTHOR:  Nandland, changes (Richard James Howe)
-- LICENSE: MIT
-- REPO:    <https://github.com/howerj/subleq-vhdl>
--
-- Taken from <https://github.com/nandland/UART>, commit
-- 4ae04682b0e59f4b9d93a4f99b06990e5bcb5cc2, it is MIT licensed as are
-- the changes, and  is smaller (and less featureful) than the UART I made at
-- <https://github.com/howerj/forth-cpu>. There are significant changes,
-- which make the UART easier to use and integrate. The original copyright
-- notice is:
--
-- -------------------------------------------------------------------------------
--
-- MIT License
-- 
-- Copyright (c) 2022 nandland
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
-- 
-- -------------------------------------------------------------------------------
-- 
-- A description of the module is available from <http://www.nandland.com>
-- as well.
-- 
library ieee, work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package uart_pkg is
	component uart_rx is
		generic (clks_per_bit: integer := 868; N: positive := 8; delay: time := 0 ns);
		port (
			clk:           in std_ulogic;
			rx_serial:     in std_ulogic;
			rx_have_data: out std_ulogic; -- high for only one clock cycle
			rx_byte:      out std_ulogic_vector(N - 1 downto 0));
	end component;

	component uart_tx is
		generic (clks_per_bit: integer := 868; N: positive := 8; delay: time := 0 ns);
		port (
			clk:        in std_ulogic;
			tx_we:      in std_ulogic;
			tx_byte:    in std_ulogic_vector(N - 1 downto 0);
			tx_active: out std_ulogic;
			tx_serial: out std_ulogic;
			tx_done:   out std_ulogic);
	end component;

	component uart_rx_tb is
	end component;

	component uart_tb is
	end component;

	pure function calc_clks_per_bit(clock_frequency: positive; baud: positive) return integer;

	procedure uart_write_byte( -- Simulation only
		baud: in positive;
		data_input_byte: in std_ulogic_vector(7 downto 0);
		signal tx_line: out std_ulogic);
end package;

package body uart_pkg is
	pure function calc_clks_per_bit(clock_frequency: positive; baud: positive) return integer is
	begin
		return clock_frequency / baud;
	end function;

	procedure uart_write_byte(
		baud: in positive;
		data_input_byte: in std_ulogic_vector(7 downto 0);
		signal tx_line: out std_ulogic) is -- Make sure to set to '1' during signal declaration.
		constant bit_period: time := 1000 ms / baud;
	begin -- Test Bench Low Level UART Write byte (pretty neat) 8N1 format only
		tx_line <= '0'; -- Send Start Bit
		wait for bit_period;
		
		for i in 0 to data_input_byte'high loop -- Send Data Byte
			tx_line <= data_input_byte(i);
			wait for bit_period;
		end loop;
		
		tx_line <= '1'; -- Send Stop Bit
		wait for bit_period;
	end;
end;

-- This file contains the UART Receiver.  This receiver is able to
-- receive 8 bits of serial data, one start bit, one stop bit,
-- and no parity bit.  When receive is complete `rx_have_data` will be
-- driven high for one clock cycle.
-- 
-- Set Generic clks_per_bit as follows:
-- clks_per_bit = (Frequency of clk)/(Frequency of UART)
-- Example: 25 MHz Clock, 115200 baud UART
-- (25000000)/(115200) = 217
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.uart_pkg.all;

entity uart_rx is
	generic (clks_per_bit: integer := 868; N: positive := 8; delay: time := 0 ns);
	port (
		clk:           in std_ulogic;
		rx_serial:     in std_ulogic;
		rx_have_data: out std_ulogic; -- High for only one clock cycle
		rx_byte:      out std_ulogic_vector(N - 1 downto 0));
end entity;

architecture rtl of uart_rx is
	type state_t is (s_idle, s_rx_start_bit, s_rx_data_bits, s_rx_stop_bit, s_cleanup);
	signal state: state_t := s_idle;
	signal clk_count: integer range 0 to clks_per_bit - 1 := 0;
	signal bit_index: integer range 0 to N - 1 := 0;
	signal r_rx_byte: std_ulogic_vector(rx_byte'range) := (others => '0');
	signal r_rx_dv:   std_ulogic := '0';
begin
	process (clk)
	begin
		if rising_edge(clk) then
			case state is
			when s_idle =>
				r_rx_dv <= '0' after delay;
				clk_count <= 0 after delay;
				bit_index <= 0 after delay;
				if rx_serial = '0' then -- Start bit detected
					state <= s_rx_start_bit after delay;
				else
					state <= s_idle after delay;
				end if;
			when s_rx_start_bit => -- Check middle of start bit to make sure it's still low
				if clk_count = (clks_per_bit - 1) / 2 then
					if rx_serial = '0' then
						clk_count <= 0 after delay; -- reset counter since we found the middle
						state <= s_rx_data_bits after delay;
					else
						state <= s_idle after delay;
					end if;
				else
					clk_count <= clk_count + 1 after delay;
					state <= s_rx_start_bit after delay;
				end if;
			when s_rx_data_bits => -- Wait clks_per_bit - 1 clock cycles to sample serial data
				if clk_count < (clks_per_bit - 1) then
					clk_count <= clk_count + 1 after delay;
					state <= s_rx_data_bits after delay;
				else
					clk_count <= 0 after delay;
					r_rx_byte(bit_index) <= rx_serial after delay;
					
					-- Check if we have sent out all bits
					if bit_index < (N - 1) then
						bit_index <= bit_index + 1 after delay;
						state <= s_rx_data_bits after delay;
					else
						bit_index <= 0 after delay;
						state <= s_rx_stop_bit after delay;
					end if;
				end if;
			when s_rx_stop_bit => -- Receive Stop bit. Stop bit = 1
				-- Wait clks_per_bit - 1 clock cycles for Stop bit to finish
				if clk_count < (clks_per_bit - 1) then
					clk_count <= clk_count + 1 after delay;
					state <= s_rx_stop_bit after delay;
				else
					r_rx_dv <= '1' after delay;
					clk_count <= 0 after delay;
					state <= s_cleanup after delay;
				end if;
			when s_cleanup => -- Stay here 1 clock
				state <= s_idle after delay;
				r_rx_dv <= '0' after delay;
			when others =>
				state <= s_idle after delay;
			end case;
		end if;
	end process;

	rx_have_data <= r_rx_dv after delay;
	rx_byte <= r_rx_byte after delay;
end RTL;

-- This file module contains the UART Transmitter.  This transmitter is able
-- to transmit 8 bits of serial data, one start bit, one stop bit,
-- and no parity bit.  When transmit is complete tx_done will be
-- driven high for one clock cycle.
--
-- Set Generic clks_per_bit as follows:
-- clks_per_bit = (Frequency of clk)/(Frequency of UART)
-- Example: 25 MHz Clock, 115200 baud UART
-- (25000000)/(115200) = 217
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.uart_pkg.all;

entity uart_tx is
	generic (clks_per_bit: integer := 868; N: positive := 8; delay: time := 0 ns);
	port (
		clk:        in std_ulogic;
		tx_we:      in std_ulogic;
		tx_byte:    in std_ulogic_vector(N - 1 downto 0);
		tx_active: out std_ulogic;
		tx_serial: out std_ulogic;
		tx_done:   out std_ulogic);
end entity;

architecture rtl of uart_tx is
	type state_t is (idle, tx_start_bit, tx_data_bits, tx_stop_bit, cleanup);
	signal state: state_t := idle;

	signal clk_count: integer range 0 to clks_per_bit - 1 := 0;
	signal bit_index: integer range 0 to tx_byte'high := 0;
	signal r_tx_data: std_ulogic_vector(tx_byte'range) := (others => '0');
	signal r_tx_done: std_ulogic := '0';
begin
	process (clk)
	begin
		if rising_edge(clk) then
			r_tx_done <= '0'; -- Default assignment
			case state is
			when idle =>
				tx_active <= '0' after delay;
				tx_serial <= '1' after delay; -- Drive Line High for Idle
				clk_count <= 0 after delay;
				bit_index <= 0 after delay;

				if tx_we = '1' then
					r_tx_data <= tx_byte after delay;
					state <= tx_start_bit after delay;
				else
					state <= idle after delay;
				end if;
			when tx_start_bit => -- Send out Start Bit. Start bit = 0
				tx_active <= '1' after delay;
				tx_serial <= '0' after delay;

				-- Wait clks_per_bit - 1 clock cycles for start bit to finish
				if clk_count < (clks_per_bit - 1) then
					clk_count <= clk_count + 1 after delay;
					state <= tx_start_bit after delay;
				else
					clk_count <= 0 after delay;
					state <= tx_data_bits after delay;
				end if;
			when tx_data_bits => -- Wait clks_per_bit - 1 clock cycles for data bits to finish
				tx_serial <= r_tx_data(bit_index) after delay;
				
				if clk_count < (clks_per_bit - 1) then
					clk_count <= clk_count + 1 after delay;
					state <= tx_data_bits after delay;
				else
					clk_count <= 0 after delay;
					
					-- Check if we have sent out all bits
					if bit_index < tx_byte'high then
						bit_index <= bit_index + 1 after delay;
						state <= tx_data_bits after delay;
					else
						bit_index <= 0 after delay;
						state <= tx_stop_bit after delay;
					end if;
				end if;
			when tx_stop_bit => -- Send out Stop bit. Stop bit = 1
				tx_serial <= '1' after delay;

				-- Wait clks_per_bit - 1 clock cycles for Stop bit to finish
				if clk_count < (clks_per_bit - 1) then
					clk_count <= clk_count + 1 after delay;
					state <= tx_stop_bit after delay;
				else
					r_tx_done <= '1' after delay;
					clk_count <= 0 after delay;
					state <= cleanup after delay;
				end if;
			when cleanup => -- Stay here 1 clock
				tx_active <= '0' after delay;
				state <= idle after delay;
			when others =>
				state <= idle after delay;
			end case;
		end if;
	end process;
	tx_done <= r_tx_done after delay;
end architecture;
----------------------------------------------------------------------
-- Tests RX Behavior
----------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;
use work.uart_pkg.all;

entity uart_rx_tb is
end uart_rx_tb;

architecture testing of uart_rx_tb is
	constant clock_frequency: positive := 100_000_000;
	constant clock_period:    time     := 1000 ms / clock_frequency;
	constant baud:            positive := 115200;
	constant clks_per_bit:    integer  := clock_frequency / baud;

	signal clk:       std_ulogic := '0';
	signal rx_byte:   std_ulogic_vector(7 downto 0) := (others => 'U');
	signal rx_serial: std_ulogic := '1';
	signal stop:      boolean    := false;
begin
	uart_rx_inst: entity work.uart_rx
		generic map (clks_per_bit => clks_per_bit)
		port map (
			clk          => clk,
			rx_serial    => rx_serial,
			rx_have_data => open,
			rx_byte      => rx_byte);

	clock_process: process
	begin
		while not stop loop
			clk <= '1';
			wait for clock_period / 2;
			clk <= '0';
			wait for clock_period / 2;
		end loop;
		wait;
	end process;
	process is
	begin
		wait until rising_edge(clk);
		uart_write_byte(baud, x"3F", rx_serial);
		wait until rising_edge(clk);

		if rx_byte = x"3F" then
			report "UART RX TB: Test Passed - Correct Byte Received";
		else 
			report "UART RX TB: Test Failed - Incorrect Byte Received" severity failure;
		end if;
		stop <= true;
		wait;
	end process;
end architecture;
----------------------------------------------------------------------
-- UART test bench
----------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;
use work.uart_pkg.all;

entity uart_tb is
end uart_tb;

architecture testing of uart_tb is
	constant clock_frequency: positive := 100_000_000;
	constant clock_period:    time     := 1000 ms / clock_frequency;
	constant baud:            positive := 115200;
	constant clks_per_bit:    integer  := clock_frequency / baud;
	
	signal clk: std_ulogic := '0';
	signal tx_dv: std_ulogic := '0';
	signal tx_byte: std_ulogic_vector(7 downto 0) := (others => '0');
	signal tx_serial, tx_done: std_ulogic := 'U';
	signal rx_dv: std_ulogic := 'U';
	signal rx_serial: std_ulogic := '1';
	signal rx_byte: std_ulogic_vector(7 downto 0) := (others => 'U');
	signal stop: boolean := false;
begin
	uart_tx_inst: entity work.uart_tx
		generic map (clks_per_bit => clks_per_bit)
		port map (
			clk       => clk,
			tx_we     => tx_dv,
			tx_byte   => tx_byte,
			tx_active => open,
			tx_serial => tx_serial,
			tx_done   => tx_done);

	uart_rx_inst: entity work.uart_rx
		generic map (clks_per_bit => clks_per_bit)
		port map (
			clk          => clk,
			rx_serial    => rx_serial,
			rx_have_data => rx_dv,
			rx_byte      => rx_byte);

	clock_process: process
	begin
		while not stop loop
			clk <= '1';
			wait for clock_period / 2;
			clk <= '0';
			wait for clock_period / 2;
		end loop;
		wait;
	end process;

	process is
	begin
		wait until rising_edge(clk);
		wait until rising_edge(clk);
		tx_dv <= '1';
		tx_byte <= x"AB";
		wait until rising_edge(clk);
		tx_dv <= '0';
		wait until tx_done = '1';

		wait until rising_edge(clk);
		uart_write_byte(baud, x"37", rx_serial);
		wait until rising_edge(clk);

		if rx_byte = x"37" then
			report "UART TB: Test Passed - Correct Byte Received";
		else 
			report "UART TB: Test Failed - Incorrect Byte Received" severity failure;
		end if;
		stop <= true;
		wait;
	end process;
end architecture;

