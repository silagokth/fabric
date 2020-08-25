-------------------------------------------------------
--! @file data_selector.vhd
--! @brief This module is used to select the access to the dimarch data bus
--! @details 
--! @author Dimitrios Stathis
--! @version 1.0
--! @date 15/01/2018
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
-- File       : data_selector.vhd
-- Author     : Dimitrios Stathis
-- Company    : KTH
-- Created    : 15/01/2018
-- Last update: 15/01/2018
-- Platform   : SiLago
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Copyright (c) 2018
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
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
--! Standard ieee library
LIBRARY ieee;
--! Standard logic library
USE ieee.std_logic_1164.ALL;
--! Standard numeric library for signed and unsigned
USE ieee.numeric_std.ALL;
USE work.noc_types_n_constants.DATA_IO_SIGNAL_TYPE;
USE work.top_consts_types_package.SRAM_WIDTH;
USE work.noc_types_n_constants.zero_block;

--! This module is used to select the access to the dimarch data bus

--! We use this module to select which of the 2 DRRA rows will have
--! access to the data bus that connects with the bottom line of the DiMArch
ENTITY data_selector IS

    PORT
    (
        data_in_this                 : IN STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);  --! data from this 
        data_in_next                 : IN STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);  --! data from other row
        data_out                     : OUT STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0); --! data out
        dimarch_silego_rd_2_out_this : IN std_logic;                                  --! ready signal from this cell
        dimarch_silego_rd_out_next   : IN std_logic                                   --! ready signal from the other row
    );
END ENTITY data_selector;

--! @brief Simple structural architecture for address assignment.
--! @details This is a simple multiplexer that decides the connection to the data bus
ARCHITECTURE RTL OF data_selector IS
BEGIN

    data_out <= data_in_this WHEN (dimarch_silego_rd_2_out_this = '1')
        ELSE
        data_in_next WHEN (dimarch_silego_rd_out_next = '1')
        ELSE
        zero_block;

END ARCHITECTURE RTL;