-------------------------------------------------------
--! @file register_row.vhd
--! @brief 
--! @details 
--! @author sadiq
--! @version 1.0
--! @date 2013-10-06
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
-- Title      : 
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : register_row.vhd
-- Author     : sadiq
-- Company    : KTH
-- Created    : 2013-10-06
-- Last update: 2013-10-06
-- Platform   : SiLago
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Copyright (c) 2013
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2013-10-06  1.0      sadiq      Created
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

LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.top_consts_types_package.ALL;
ENTITY register_row IS
    --generic  (initial_vector : signed(REG_FILE_DATA_WIDTH-1 downto 0));
    PORT
    (
        rst_n   : IN std_logic;
        clk     : IN std_logic;
        wr_enb  : IN std_logic;
        reg_in  : IN signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
        reg_out : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0));
END register_row;

ARCHITECTURE behavioral OF register_row IS

BEGIN
    PROCESS (rst_n, clk, reg_in)
    BEGIN
        IF rst_n = '0' THEN
            reg_out <= (OTHERS => '0');
        ELSIF clk'event AND clk = '1' THEN
            IF wr_enb = '1' THEN
                reg_out <= reg_in;
            END IF;
        END IF;
    END PROCESS;
END behavioral;