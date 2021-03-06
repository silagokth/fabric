-------------------------------------------------------
--! @file hw_setting.vhd
--! @brief General HW settings 
--! @details This file contains the HW configuration. To change the configuration of the
--! fabric alter this file accordingly. !!! N.B. !!! The current version might need changes
--! in other files as well.
--! @author Dimitrios Stathis
--! @version 1.0
--! @date 2020-01-24
--! @bug NONE
--! @todo NONE
--! @copyright  GNU Public License [GPL-3.0].
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
-- Title      : UnitX
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : hw_setting.vhd
-- Author     : Dimitrios Stathis
-- Company    : KTH
-- Created    : 2020-01-24
-- Last update: 2020-01-24
-- Platform   : SiLago
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Copyright (c) 2020
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2020-01-24  1.0      Dimitrios Stathis      Created
-------------------------------------------------------------------------------

--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
--                                                                         #
--This file is part of SiLago.                                             #
--                                                                         #
--    SiLago platform source code is distributed freely: you can           #
--    redistribute it and/or modify it under the terms of the GNU          #
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

PACKAGE hw_setting IS

    CONSTANT HW_INSTR_DEPTH           : NATURAL := 64;
    CONSTANT HW_RAM_DEPTH             : NATURAL := 128;
    CONSTANT HW_REG_FILE_DEPTH        : NATURAL := 64;
    CONSTANT HW_COLUMNS               : NATURAL := 8;
    CONSTANT HW_ROWS                  : NATURAL := 2;
    CONSTANT HW_DIMARCH_ROWS          : NATURAL := 1;
    CONSTANT HW_RACCU_REGFILE_DEPTH   : NATURAL := 8;
    CONSTANT HW_MAX_NO_OF_RACCU_LOOPS : NATURAL := 4;
    CONSTANT HW_DPU_CONSTANT_WIDTH    : NATURAL := 8;

END PACKAGE hw_setting;

-- Some fixed parameters:
--  * BITWIDTH = 16
--  * RACCU maximum iteration is 64 (It is dependent to the RACCU operand width).
--  * RACCU_REG_BITWIDTH = 6
--  * ...

-- Note:
--   If there is a need for bigger register file then register_file.vhd and also
--   STARTING_ADDRS and NR_OF_ADDRS in top_consts package should be modified.