-------------------------------------------------------
--! @file
--! @brief Global package with the global hardware generics
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
-- Title      : hw_setting
-- Project    : SiLago
-- Supervisor : Dimitrios Stathis
-------------------------------------------------------------------------------
-- File       : hw_setting.vhd
-- Author     : 
-- Company    : KTH
-- Created    : 
-- Last update: 2019-02-25
-- Platform   : SiLago
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Copyright (c) 2019
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  		Description
-- 			   1.0      			    Created
-- 2019-02-25  1.1      Dimitrios		Comments and License
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

--! @brief HW settings package.
--! @details This package contains the configuration settings
--! for the hardware architecture.
PACKAGE hw_setting IS

	CONSTANT HW_INSTR_DEPTH    			: natural := 64;
	CONSTANT HW_RAM_DEPTH     			: natural := 128;
	CONSTANT HW_REG_FILE_DEPTH 			: natural := 64;
	CONSTANT HW_COLUMNS        			: natural := 8;
	CONSTANT HW_ROWS           			: natural := 2;
	CONSTANT HW_DIMARCH_ROWS			: natural := 1;
	CONSTANT HW_RACCU_REGFILE_DEPTH 	: natural := 8;
  	CONSTANT HW_MAX_NO_OF_RACCU_LOOPS 	: natural := 4;
  	CONSTANT HW_DPU_CONSTANT_WIDTH 		: natural := 8;		

END PACKAGE hw_setting;

-- Some fixed parameters:
--  * BITWIDTH = 16
--  * RACCU maximum iteration is 64 (It is dependent to the RACCU operand width).
--  * RACCU_REG_BITWIDTH = 6
--  * ...

-- Note:
--   If there is a need for bigger register file then register_file.vhd and also
--   STARTING_ADDRS and NR_OF_ADDRS in top_consts package should be modified.
