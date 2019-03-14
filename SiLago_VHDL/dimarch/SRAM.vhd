-------------------------------------------------------
--! @file
--! @brief SRAM Tile
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
-- Title      : SRAM
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : SRAM.vhd
-- Author     : Muhammad Ali Shami <shami@kth.se>
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
--             1.0      Muhammad Ali Shami        Created
--             1.5                                Segmented bus is replaced with buffers
-- 2019-03-11  2.0      Dimitrios Stathis         Update SRAM module into async and removed extra signals
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

--! IEEE library
LIBRARY IEEE;
--! Use standard library
USE IEEE.std_logic_1164.ALL;
--! Use numeric library for arithmetic operations
USE ieee.numeric_std.ALL;
--! Include the sram address width
use work.top_consts_types_package.SRAM_ADDRESS_WIDTH;
--! Include sram depth
use work.top_consts_types_package.SRAM_DEPTH;
--! Include the sram width
use work.top_consts_types_package.SRAM_WIDTH;
--! Use the noc package, includes all the type and constant definition 
use work.noc_types_n_constants.all;
--! Use the misc package
USE work.misc.ALL;
--! Include the number of dimarch columns
USE work.top_consts_types_package.COLUMNS;
--! Include the bidwidt from the top package
USE work.top_consts_types_package.BITWIDTH;
--! Include the textio package for read and write files (not in use)
USE STD.TEXTIO.ALL;

--! @brief This is a Generic SRAM simulation model.
--! @detail The Depth and width of the simulation model
--! be changed by changing the generics. Its a two port Memory, with one read and one write port
entity SRAM is
	--GENERIC(
	--	This_ROW : UNSIGNED(ROW_WIDTH - 1 DOWNTO 0) := (OTHERS => '0');
	--	This_COL : UNSIGNED(COL_WIDTH - 1 DOWNTO 0) := (OTHERS => '0'));
	port(
	--	rst_n    : IN  STD_LOGIC;
	--	clk      : IN  STD_LOGIC;
		en_w     : in  STD_LOGIC;       -- Write Enable
		addrs_w  : in  STD_LOGIC_VECTOR(SRAM_ADDRESS_WIDTH - 1 downto 0); -- Write Address
		inp      : in  STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0); -- Input
		en_r     : in  STD_LOGIC;       -- Read Enable
		addrs_r  : in  STD_LOGIC_VECTOR(SRAM_ADDRESS_WIDTH - 1 downto 0); -- Read Address
		outp     : out STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
	--	tb_en    : IN  STD_LOGIC;       -- Write Enable from test bench
	--	tb_addrs : IN  STD_LOGIC_VECTOR(SRAM_ADDRESS_WIDTH - 1 downto 0); -- Write Address from test bench
	--	tb_inp   : IN  STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
	--	tb_ROW   : IN  UNSIGNED(ROW_WIDTH - 1 DOWNTO 0);
	--	tb_COL   : IN  UNSIGNED(COL_WIDTH - 1 DOWNTO 0) -- Input from test bench
	);                                  -- Ouput
end SRAM;

--dc::synthesis off
--! @brief Architecture of the SRAM
--! @detail This SRAM architecture is just for simulation, it acts as asynch SRAM
architecture SRAM_behaviour of SRAM is
	type mem_type is array (0 to SRAM_DEPTH - 1) of std_logic_vector(SRAM_WIDTH - 1 downto 0);

begin                                   -- beh
	-- synopsys synthesis_off
	rw : process(en_w, addrs_w, inp, en_r, addrs_r)--, rst_n, tb_COL, tb_ROW, tb_addrs, tb_en, tb_inp, clk)
		variable mem    : mem_type;

	begin                               -- process rd
		--if rst_n = '0' then
		--	outp <= (others => '0');
--			mem :=(others =>(others=>'0'));
		--elsif tb_en = '1' and tb_COL = This_COL and tb_ROW = This_ROW then
		--	mem(to_integer(unsigned(tb_addrs))) := tb_inp;
		--els
		if en_w = '1' then --and clk'event and clk = '1' then
			mem(to_integer(unsigned(addrs_w))) := inp;
		end if;
		
		if en_r = '1' then
			outp <= mem(to_integer(unsigned(addrs_r)));
		else
			outp <= mem(to_integer(unsigned(addrs_r)));
		end if;
	end process rw;

-- synopsys synthesis_on
end SRAM_behaviour;

--dc::synthesis on

