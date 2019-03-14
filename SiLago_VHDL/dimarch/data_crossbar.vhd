-------------------------------------------------------
--! @file
--! @brief data_crossbar
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
-- Title      : data_crossbar
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : data_crossbar.vhd
-- Author     : Muhammad Adeel Tajammul
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
--            1.0      Muhammad Adeel Tajammul   Created
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
-- This is the bi-directional version of the data crossbar 
-------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
--USE work.noc_types_n_constants.ALL;
use work.top_consts_types_package.SRAM_WIDTH;
use work.crossbar_types_n_constants.all;
entity data_crossbar is
	generic(NumberofPorts : integer := nr_of_crossbar_ports);
	port(
		rst_n          : in  std_logic;
		clk            : in  std_logic;
		DIRECTION      : IN  CORSSBAR_INSTRUCTION_RECORD_TYPE;
		--		VER_SELECT : IN CORSSBAR_INSTRUCTION_RECORD_TYPE;

		DATA_MEM_IN    : IN  STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
		DATA_NORTH_IN  : IN  STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
		DATA_SOUTH_IN  : IN  STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
		DATA_EAST_IN   : IN  STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
		DATA_WEST_IN   : IN  STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
		DATA_MEM_OUT   : OUT STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
		DATA_NORTH_OUT : OUT STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
		DATA_SOUTH_OUT : OUT STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
		DATA_EAST_OUT  : OUT STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
		DATA_WEST_OUT  : OUT STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0)
	);
end entity data_crossbar;

architecture RTL of data_crossbar is
	TYPE data_array IS ARRAY (NATURAL RANGE <>) OF STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
	TYPE inps_type IS ARRAY (nr_of_crossbar_ports DOWNTO 0) OF data_array(nr_of_crossbar_ports - 1 DOWNTO 0);
	CONSTANT MEMORY : INTEGER := 0;
	CONSTANT NORTH  : INTEGER := 1;
	CONSTANT EAST   : INTEGER := 2;
	CONSTANT WEST   : INTEGER := 3;
	CONSTANT SOUTH  : INTEGER := 4;

	SIGNAL input   : data_array(nr_of_crossbar_ports DOWNTO 0);
	SIGNAL inps    : inps_type;
	SIGNAL selects : CROSSBAR_select_type;
begin

	input(MEMORY) <= DATA_MEM_IN;
	input(NORTH)  <= DATA_NORTH_IN;
	input(EAST)   <= DATA_EAST_IN;
	input(WEST)   <= DATA_WEST_IN;
	input(SOUTH)  <= DATA_SOUTH_IN;
	--TODO Check this generate
	G_SEL_INPS : FOR i IN 0 TO NumberofPorts GENERATE
		inps(i) <= input(NumberofPorts DOWNTO i + 1) when (i = 0)
			else input(i - 1 DOWNTO 0) when (i = NumberofPorts)
			else input(NumberofPorts DOWNTO i + 1) & input(i - 1 DOWNTO 0);
	END GENERATE;

	p_OUTPUT_REG : PROCESS(rst_n, clk)
	BEGIN
		IF rst_n = '0' THEN

			DATA_MEM_OUT   <= (OTHERS => '0');
			DATA_NORTH_OUT <= (OTHERS => '0');
			DATA_EAST_OUT  <= (OTHERS => '0');
			DATA_WEST_OUT  <= (OTHERS => '0');
			DATA_SOUTH_OUT <= (OTHERS => '0');

		ELSIF rising_edge(clk) THEN

			DATA_MEM_OUT   <= inps(MEMORY)(TO_INTEGER(UNSIGNED(selects(MEMORY))));
			DATA_NORTH_OUT <= inps(NORTH)(TO_INTEGER(UNSIGNED(selects(NORTH))));
			DATA_EAST_OUT  <= inps(EAST)(TO_INTEGER(UNSIGNED(selects(EAST))));
			DATA_WEST_OUT  <= inps(WEST)(TO_INTEGER(UNSIGNED(selects(WEST))));
			DATA_SOUTH_OUT <= inps(SOUTH)(TO_INTEGER(UNSIGNED(selects(SOUTH))));

		END IF;

	END PROCESS p_OUTPUT_REG;

	p_select : process(clk, rst_n) is
	begin
		if rst_n = '0' then
			selects <= (others => (otherS => '0'));
		elsif rising_edge(clk) then
			if DIRECTION.ENABLE = '1' then
				selects(to_integer(unsigned(DIRECTION.SELECT_TO))) <= DIRECTION.SELECT_FROM;
			end if;
			--  		if VER_SELECT.ENABLE = '1' then  		  		selects(to_integer(unsigned(VER_SELECT.SELECT_TO)))<= VER_SELECT.SELECT_FROM;end if;
		end if;
	end process p_select;

end architecture RTL;

