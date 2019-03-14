-------------------------------------------------------
--! @file
--! @brief sram_agu
-------------------------------------------------------
---------------- Copyright (c) notice -----------------------------------------
--
-- The VHDL code, the logic and concepts described in this file constitute
-- the intellectual property of the authors listed below, who are affiliated
-- to KTH(Kungliga Tekniska Högskolan), School of ICT, Kista.
-- Any unauthorised use, copy or distribution is strictly prohibited.
-- Any authorised use, copy or distribution should carry this copyright notice
-- unaltered.
-------------------------------------------------------------------------------
-- Title      : sram_agu
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : sram_agu.vhd
-- Author     : Muhammad Ali Shami
-- Company    : KTH
-- Created    : 
-- Last update: 2019-03-11
-- Platform   : SiLago
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Copyright (c) 2014 
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  		          Description
--            1.0      Muhammad Ali Shami         Created
-------------------------------------------------------------------------------

--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
--                                                                         #
--This file is part of SiLago.                                             #
--                                                                         #
--    SiLago platform source code is distributed freely: you can           #
--    redistribute it and/or modify it under the terms of the GNU    	   #
--    General Public License as published by the Free Software Foundation, #
--    either version 3 of the License, or (at your option) any             #
--    later version.                                                       #
--                                                                         #
--    SiLago is distributed in the hope that it will be useful,            #
--    but WITHOUT ANY WARRANTY; without even the implied warranty of       #
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        #
--    GNU General Public License for more details.                         #
--                                                                         #
--    You should have received a copy of the GNU General Public License    #
--    along with SiLago.  If not, see <https://www.gnu.org/licenses/>.     #
--                                                                         #
--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

-------------------------------------------------------------------------------
-- This is a single port rd/wr agu.
-------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;
--use work.util_package.all;

use work.top_consts_types_package.all;
use work.noc_types_n_constants.sram_agu_instruction_type;
use work.noc_types_n_constants.sram_instr_zero;

entity sram_agu is                      --(RD/WR)

	port(
		rst_n        : in  std_logic;
		clk          : in  std_logic;
		instr        : in  sram_agu_instruction_type;
		rw           : out std_logic;
		reorder      : out std_logic;
		rw_addrs_out : out unsigned(RAM_ADDRESS_WIDTH - 1 downto 0)
	);

end sram_agu;

architecture behv_rtl of sram_agu is
	signal instr_reg                         : sram_agu_instruction_type;
	-- interation count signals and flags
	signal loop1_count_reg                   : unsigned(sr_loop1_iteration_width - 1 downto 0);
	signal loop1_count_up, loop1_reset       : std_logic;
	signal loop2_count_reg                   : unsigned(sr_loop2_iteration_width - 1 downto 0);
	signal loop2_count_up, loop2_reset       : std_logic;
	--- delay signals and flags
	signal loop1_delay_reg                   : unsigned(sr_loop1_iteration_width - 1 downto 0);
	signal loop1_delay_en, loop1_delay_reset : std_logic;
	signal loop2_delay_reg                   : unsigned(sr_loop2_iteration_width - 1 downto 0);
	signal loop2_delay_en, loop2_delay_reset : std_logic;

	signal initial_delay_reg : unsigned(sr_loop1_iteration_width - 1 downto 0);
	signal initial_delay_en  : std_logic;

	------------------------ address datapath signals----------------

	signal address_adder_input1, address_adder_input2, address_adder_result, address_reg : unsigned(RAM_ADDRESS_WIDTH downto 0);
	signal loop1_increment                                                               : unsigned(RAM_ADDRESS_WIDTH downto 0);
	signal loop2_increment                                                               : unsigned(RAM_ADDRESS_WIDTH downto 0);

	signal initial_select_input1         : std_logic; -- if 1 then select initial address else adder_reg
	signal increment_select_input2       : std_logic; -- if 0 then select loop1 increment else loop2 increment 
	signal address_initial_select_output : std_logic; -- if 1 then select initial address else select adder output
	signal retain_address_output         : std_logic; -- if 1 then retain 1 then retain output value else load adder output
	------------------------------------ fsm  states ------------------
	type state is (idle, initial_state, loops);
	signal current_state, next_state : state;
	signal rw_flag                   : std_logic;
	signal complete                  : std_logic;
BEGIN
reorder  <= instr_reg.agu_mode;
	---------------- Register new instruction-------------------------
	p_instr_reg : process(clk, rst_n) is
	begin
		if rst_n = '0' then
			instr_reg <= sram_instr_zero;
		elsif rising_edge(clk) then
			if complete = '1' then
				instr_reg <= instr;
				
			elsif instr_reg.enable = '1' THEN -- after one cycle make enable zero 
				instr_reg.enable <= '0';
			END IF;
			if initial_select_input1 = '1' and increment_select_input2 = '1' then -- update initial address for loop2 iterations
				instr_reg.Initial_Address <= address_adder_result(sr_initial_address_width - 1 downto 0);
			end if;
		end if;
	end process p_instr_reg;
	---------------- loop1 counter-------------------------
	p_loop1_counter : process(clk, rst_n) is
	begin
		if rst_n = '0' then
			loop1_count_reg <= (others => '0');
		elsif rising_edge(clk) then
			if loop1_reset = '1' then   -- syncronous reset to avoid latch
				loop1_count_reg <= (others => '0');
			elsif loop1_count_up = '1' then
				loop1_count_reg <= loop1_count_reg + 1;
			end if;

		end if;
	end process;
	---------------- loop2 counter-------------------------
	p_loop2_counter : process(clk, rst_n) is
	begin
		if rst_n = '0' then
			loop2_count_reg <= (others => '0');
		elsif rising_edge(clk) then
			if loop2_reset = '1' then   -- syncronous reset to avoid latch 
				loop2_count_reg <= (others => '0');
			elsif loop2_count_up = '1' then
				loop2_count_reg <= loop2_count_reg + 1;
			end if;
		end if;
	end process;
	---------------- loop1 delay-------------------------
	p_loop1_delay : process(clk, rst_n) is
	begin
		if rst_n = '0' then
			loop1_delay_reg <= (others => '0');
		elsif rising_edge(clk) then
			if loop1_delay_en = '1' then
				loop1_delay_reg <= loop1_delay_reg + 1;
			else
				loop1_delay_reg <= (others => '0');
			end if;
		end if;
	end process;
	---------------- loop2 delay-------------------------
	p_loop2_delay : process(clk, rst_n) is
	begin
		if rst_n = '0' then
			loop2_delay_reg <= (others => '0');
		elsif rising_edge(clk) then
			if loop2_delay_en = '1' then
				loop2_delay_reg <= loop2_delay_reg + 1;
			else
				loop2_delay_reg <= (others => '0');
			end if;
		end if;
	end process;
	---------------- initial delay-------------------------
	p_initial_delay : process(clk, rst_n) is
	begin
		if rst_n = '0' then
			initial_delay_reg <= (others => '0');
		elsif rising_edge(clk) then
			if initial_delay_en = '1' then
				initial_delay_reg <= initial_delay_reg + 1;
			else
				initial_delay_reg <= (others => '0');
			end if;
		end if;
	end process;
	------------------------ address datapath----------------
	---- Adder input1 mux 
	address_adder_input1 <= address_reg when (initial_select_input1 = '0') else instr_reg.Initial_Address(RAM_ADDRESS_WIDTH - 1) & instr_reg.Initial_Address;
	---- Adder input2 mux
	address_adder_input2 <= loop1_increment when (increment_select_input2 = '0') else loop2_increment;

	----- Adder 
	loop1_increment      <= unsigned(instr_reg.Loop1_increment);
	loop2_increment      <= unsigned(instr_reg.Loop2_increment);
	address_adder_result <= address_adder_input1 + address_adder_input2;

	--- output register
	p_address_reg : process(clk, rst_n) is
	begin
		if rst_n = '0' then
			address_reg <= (others => '0');
		elsif rising_edge(clk) then
			if address_initial_select_output = '1' then
				address_reg <= instr_reg.Initial_Address(RAM_ADDRESS_WIDTH - 1) & instr_reg.Initial_Address; -- assign sign extented 
			elsif retain_address_output = '0' then
				address_reg <= address_adder_result;
			end if;
		end if;
	end process p_address_reg;
	rw_addrs_out <= address_reg(RAM_ADDRESS_WIDTH - 1 downto 0);
	----------------------------state register --------------
	p_state_reg : process(clk, rst_n) is
	begin
		if rst_n = '0' then
			current_state <= idle;
			rw            <= '0';
		elsif rising_edge(clk) then
			current_state <= next_state;
			rw            <= rw_flag;
		end if;
	end process p_state_reg;
	--------------------------- next state logic --------------
	p_next_state : process(instr_reg, initial_delay_reg, loop1_count_reg, loop1_delay_reg, loop2_count_reg, loop2_delay_reg, current_state) is
	begin
		rw_flag                       <= '0';
		initial_delay_en              <= '0';
		address_initial_select_output <= '0';
		loop1_delay_en                <= '0';
		loop2_delay_en                <= '0';
		loop1_count_up                <= '0';
		loop1_reset                   <= '0';
		loop2_count_up                <= '0';
		loop2_reset                   <= '0';
		loop1_delay_reset             <= '0';
		loop2_delay_reset             <= '0';
		retain_address_output         <= '1';
		increment_select_input2       <= '0';
		initial_select_input1         <= '0';
		complete                      <= '0';
		case current_state is
			when idle =>
				if (instr_reg.enable = '1') then -- new instruction 
					if instr_reg.Initial_Delay /= 0 then -- check if their is initial delay 
						initial_delay_en <= '1';
						next_state       <= initial_state;
					else                -- initial delay is 0 go stright to loop1
						rw_flag                       <= '1';
						address_initial_select_output <= '1'; -- load initial address in address_reg
						initial_select_input1         <= '1';
						next_state                    <= loops;
						--assert false report "start_loop" severity note;
						
					end if;
				else
					next_state <= idle;
					complete   <= '1';
				end if;
			when initial_state =>
				if initial_delay_reg < instr_reg.Initial_Delay then -- still in initial delay
					initial_delay_en <= '1';
					next_state       <= initial_state;
				else                    -- initial delay is complete goto loop1
					next_state <= loops;

					-- skip datapath and use initial address
					rw_flag                       <= '1';
					address_initial_select_output <= '1';
					initial_select_input1         <= '1';
				end if;
			when loops =>
				if loop1_count_reg < instr_reg.Loop1_interations then --  in inner loop iterations
					if loop1_delay_reg < instr_reg.Loop1_Delay then -- in loop1 delay
						next_state <= loops;

						loop1_delay_en <= '1'; -- increment loop1 delay


					else                -- loop1_iteration, reset loop1_delay
						next_state            <= loops;
						rw_flag               <= '1';
						loop1_count_up        <= '1'; -- increment loop1 counter
						retain_address_output <= '0'; -- do not retain output and load from adder 

					end if;
				elsif loop2_count_reg < instr_reg.Loop2_iterations then -- in outer loop iterations
					if loop2_delay_reg < instr_reg.Loop2_Delay then -- in loop2 delay
						loop2_delay_en <= '1';
						next_state     <= loops;

					else                -- loop2 iteration and reset loop1
						loop1_reset             <= '1';
						loop2_count_up          <= '1';
						next_state              <= loops;
						------r/w first value of next iteration ------------
						rw_flag                 <= '1';
						retain_address_output   <= '0'; -- do not retain output and load from adder
						increment_select_input2 <= '1'; -- add loop2 increment
						initial_select_input1   <= '1'; -- with loop2 index

					end if;

				else                    -- all iterations complete
--					assert false report "completed iterations" severity error;
					loop1_reset <= '1';
					loop2_reset <= '1';
					next_state  <= idle;
					complete    <= '1';
				end if;

		end case;
	end process p_next_state;

end architecture;

