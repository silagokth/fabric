-------------------------------------------------------
--! @file
--! @brief crossbar_types_n_constants
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
-- Title      : crossbar_types_n_constants
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : crossbar_types_n_constants.vhd
-- Author     : 
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
LIBRARY ieee;
use ieee.std_logic_1164.ALL; 
use ieee.numeric_std.ALL;
--use ieee.std_logic_arith.conv_std_logic_vector;
use work.noc_types_n_constants.ROW_WIDTH;
use work.top_consts_types_package.SRAM_WIDTH;
use work.util_package.log2;
--use work.noc_types_n_constants.  

package crossbar_types_n_constants is
	--------------------------------------------------------------------
-- CROSSBAR 
--------------------------------------------------------------------
CONSTANT nr_of_crossbar_ports	: INTEGER := 5;
CONSTANT crossbar_select_width  : INTEGER := log2(nr_of_crossbar_ports);
 
TYPE CROSSBAR_DATA_TYPE				IS ARRAY ( NATURAL 	RANGE <>) OF STD_LOGIC_VECTOR (SRAM_WIDTH-1 downto 0);




--------------------------------------------------------------------
-- CROSSBAR DIRECTIONS 
--------------------------------------------------------------------
TYPE CROSSBAR_select_type			IS ARRAY ( nr_of_crossbar_ports-1 downto  0) OF STD_LOGIC_VECTOR (crossbar_select_width-1 DOWNTO 0);

TYPE CORSSBAR_INSTRUCTION_RECORD_TYPE IS RECORD
	ENABLE		: 	STD_LOGIC;
	SELECT_FROM :	STD_LOGIC_VECTOR (crossbar_select_width-1 DOWNTO 0);	
	SELECT_TO	:	STD_LOGIC_VECTOR (crossbar_select_width   DOWNTO 0);
END RECORD;
constant to_memory : std_logic_vector(crossbar_select_width downto 0):=(others => '0');
constant to_north  : std_logic_vector(crossbar_select_width downto 0):=STD_LOGIC_VECTOR(to_unsigned(1,crossbar_select_width+1));
constant to_east   : std_logic_vector(crossbar_select_width downto 0):=STD_LOGIC_VECTOR(to_unsigned(2,crossbar_select_width+1));
constant to_west   : std_logic_vector(crossbar_select_width downto 0):=STD_LOGIC_VECTOR(to_unsigned(3,crossbar_select_width+1));
constant to_south  : std_logic_vector(crossbar_select_width downto 0):=STD_LOGIC_VECTOR(to_unsigned(4,crossbar_select_width+1));


constant from_port_0 : std_logic_vector(crossbar_select_width-1 downto 0):=std_logic_vector(to_unsigned(0,crossbar_select_width));
constant from_port_1 : std_logic_vector(crossbar_select_width-1 downto 0):=std_logic_vector(to_unsigned(1,crossbar_select_width));
constant from_port_2 : std_logic_vector(crossbar_select_width-1 downto 0):=std_logic_vector(to_unsigned(2,crossbar_select_width));
constant from_port_3 : std_logic_vector(crossbar_select_width-1 downto 0):=std_logic_vector(to_unsigned(3,crossbar_select_width));

CONSTANT IGNORE					: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('0',from_port_0,to_memory );

CONSTANT FROM_NORTH_TO_MEMORY	: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('1',from_port_0,to_memory );
CONSTANT FROM_EAST_TO_MEMORY	: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('1',from_port_1,to_memory );
CONSTANT FROM_WEST_TO_MEMORY	: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('1',from_port_2,to_memory );
CONSTANT FROM_SOUTH_TO_MEMORY	: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('1',from_port_3,to_memory );

CONSTANT FROM_MEMORY_TO_NORTH	: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('1',from_port_0,to_north );
CONSTANT FROM_EAST_TO_NORTH		: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('1',from_port_1,to_north );
CONSTANT FROM_WEST_TO_NORTH		: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('1',from_port_2,to_north );
CONSTANT FROM_SOUTH_TO_NORTH	: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('1',from_port_3,to_north );

CONSTANT FROM_MEMORY_TO_EAST	: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('1',from_port_0,to_east );
CONSTANT FROM_NORTH_TO_EAST		: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('1',from_port_1,to_east );
CONSTANT FROM_WEST_TO_EAST 		: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('1',from_port_2,to_east );
CONSTANT FROM_SOUTH_TO_EAST 	: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('1',from_port_3,to_east );

CONSTANT FROM_MEMORY_TO_WEST 	: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('1',from_port_0,to_west );
CONSTANT FROM_NORTH_TO_WEST 	: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('1',from_port_1,to_west );
CONSTANT FROM_EAST_TO_WEST 		: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('1',from_port_2,to_west );
CONSTANT FROM_SOUTH_TO_WEST 	: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('1',from_port_3,to_west );                                                                                                                                                                       

CONSTANT FROM_MEMORY_TO_SOUTH 	: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('1',from_port_0,to_south );
CONSTANT FROM_NORTH_TO_SOUTH 	: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('1',from_port_1,to_south );                                                                                                                                                                       
CONSTANT FROM_EAST_TO_SOUTH 	: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('1',from_port_2,to_south );
CONSTANT FROM_WEST_TO_SOUTH 	: CORSSBAR_INSTRUCTION_RECORD_TYPE :=('1',from_port_3,to_south );

		
end package crossbar_types_n_constants;
