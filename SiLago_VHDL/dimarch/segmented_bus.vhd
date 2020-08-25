-------------------------------------------------------
--! @file segmented_bus.vhd
--! @brief 
--! @details 
--! @author Muhammad Adeel Tajammul
--! @version 1.0
--! @date 
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
-- File       : segmented_bus.vhd
-- Author     : Muhammad Adeel Tajammul
-- Company    : KTH
-- Created    : 
-- Last update: 
-- Platform   : SiLago
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Copyright (c) 2014
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
--             1.0      Muhammad Adeel Tajammul      Created
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

---------------- Copyright (c) notice -----------------------------------------
-- This is a segment of bus that implements without  
-- instruction value 2 means right to left
-- instruction value 1 means left to right
-- instruction value 0 means break
-- instruction value 3 not used
-----------------------------------------------------------------------------------
--   DIAGRAM
-----------------------------------------------------------------------------------
--
--
--
--
--
--
--
--                                                                          
-- PARTITION_INST_left(i,j)  ______________________________________________   PARTITION_INST_right(i+1,j)   ___________________________________ PARTITION_INST_left(i+2,j)
--               ----------->|left inst                          right inst |<---------------------------- |left_inst_out        right_inst_out| ----------->left inst
--                           |         Bus segment                          |                              |                                   | SRAM_INST_hor_left_in(i,1)
--SRAM_INST_hor_left_in(i,j) |                                              | SRAM_INST_hor_right_in(i+1,j)|hor_bus_left_out  hor_bus_right_out| ----------->left in
--               ----------->|left in    SEGMENT_BUS               right in |<---------------------------- |           STILE                   |
--                           |         Bus segment                          |                              |                                   |
--SRAM_INST_hor_left_out(i,j)|                                              |SRAM_INST_hor_right_out(i+1,j)|hor_bus_left_in    hor_bus_right_in|SRAM_INST_hor_left_out(i+1,j)
--              <------------|left out                            right out |----------------------------> |___________________________________| <------------left out
--                           |______________________________________________|                                               ^ east_splitter_direction
--                                             |                                                                            |
--                                             |                                                                            |
--                                             |                                                                            |
--                                             |status                                                                      |
--           __________________________________|____________________________________________________________________________|

-- PARTITION_INST_right(i,j)     ___________________________________    PARTITION_INST_left(i,j)       ______________________________________________   PARTITION_INST_right(i+1,j)   ___________________________________ PARTITION_INST_left(i+1,j)   
--<---------------------------- |left_inst_out        right_inst_out| ------------------------------->|left inst                          right inst |<---------------------------- |left_inst_out        right_inst_out| ----------->left inst       
--                              |                                   |                                 |         Bus segment                          |                              |                                   | SRAM_INST_hor_left_in(i+1,1)  
-- SRAM_INST_hor_right_in(i,j)  |hor_bus_left_out  hor_bus_right_out|  SRAM_INST_hor_left_in(i,j)     |                                              | SRAM_INST_hor_right_in(i+1,j)|hor_bus_left_out  hor_bus_right_out| ----------->left in         
--<---------------------------- |           STILE                   |-------------------------------->|left in    SEGMENT_BUS               right in |<---------------------------- |           STILE                   |                             
--                              |                                   |                                 |         Bus segment                          |                              |                                   |                             
--SRAM_INST_hor_right_out(i,j)|hor_bus_left_in      hor_bus_right_in|  SRAM_INST_hor_left_out(i,j)    |                                              |SRAM_INST_hor_right_out(i+1,j)|hor_bus_left_in    hor_bus_right_in|SRAM_INST_hor_left_out(i+1,j)
------------------------------> |___________________________________| <-------------------------------|left out                            right out |----------------------------> |___________________________________| <------------left out       
--                                               ^ west_splitter_direction                            |______________________________________________|                                               ^ east_splitter_direction                        
--                                               |                                                                                             |                                                                            |                                                
--                                               |                                                                                             |                                                                            |                                                
--                                               |                                                                                             |                                                                            |                                                
--                                               |                                                                                             |status                                                                      |                                                
--|                                               _____________________________________________________________________________________________|____________________________________________________________________________|                                                
--
--
--
--  ___________________________________    PARTITION_INST_top(i,0)       ______________________________________________   PARTITION_INST_bottom(i,j-1)   ___________________________________ PARTITION_INST_left(i+1,j)   
-- |                top_instruction_out| ------------------------------->|left inst                          right inst |<---------------------------- |bottom_inst_out        top_inst_out| ----------->left inst       
-- |                                   |                                 |         Bus segment                          |                              |                                   | SRAM_INST_ver_left_in(i+1,1)  
-- |                    ver_bus_top_out|  SRAM_INST_ver_left_in(i,0)     |                                              | SRAM_INST_ver_right_in(i,j-1)|ver_bus_bottom_out  ver_bus_top_out| ----------->left in         
-- |           contile                 |-------------------------------->|left in    SEGMENT_BUS               right in |<---------------------------- |           STILE                   |                             
-- |                                   |                                 |         Bus segment                          |                              |                                   |                             
-- |                     ver_bus_top_in|  SRAM_INST_ver_left_out(i,0)    |                                              |SRAM_INST_ver_right_out(i,j-1)|ver_bus_bottom_in    ver_bus_top_in|SRAM_INST_ver_left_out(i+1,j)
-- |___________________________________| <-------------------------------|left out                            right out |----------------------------> |___________________________________| <------------left out       
--              ^ north_splitter_direction                               |______________________________________________|                                               ^ east_splitter_direction                        
--              |                                                                                             |                                                                            |                                                
--              |                                                                                             |                                                                            |                                                
--              |                                                                                             |                                                                            |                                                
--              |                                                                     bus_direction_ver       |status                                                                      |                                                
--              |_____________________________________________________________________________________________|____________________________________________________________________________|                                                
--
--
--
--
--
--
---------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.noc_types_n_constants.ALL;
ENTITY segmented_bus IS
    PORT
    (
        clk               : IN std_logic;
        rst               : IN std_logic;
        left_instruction  : IN PARTITION_INSTRUCTION_RECORD_TYPE;
        right_instruction : IN PARTITION_INSTRUCTION_RECORD_TYPE;
        left_in           : IN NOC_BUS_TYPE;
        right_in          : IN NOC_BUS_TYPE;
        left_out          : OUT NOC_BUS_TYPE;
        right_out         : OUT NOC_BUS_TYPE;
        bus_direction     : OUT std_logic_vector(1 DOWNTO 0)
    );
END ENTITY segmented_bus;

ARCHITECTURE RTL OF segmented_bus IS
    SIGNAL direction : std_logic_vector(1 DOWNTO 0);
BEGIN
    p_segment : PROCESS (clk, rst) IS
    BEGIN
        IF rst = '0' THEN
            --			left_out  <= IDLE_BUS;
            --			right_out  <= IDLE_BUS;
            direction <= (OTHERS => '0');
        ELSIF rising_edge(clk) THEN
            IF left_instruction.ENABLE = '1' THEN
                IF left_instruction.PARTITION = "10" THEN
                    direction <= left_instruction.PARTITION;
                ELSIF left_instruction.PARTITION = "01" THEN
                    direction <= left_instruction.PARTITION;
                ELSE -- invalid instruction
                    ASSERT false REPORT "invalid partitioning" SEVERITY error;
                END IF;
            ELSIF right_instruction.ENABLE = '1' THEN
                IF right_instruction.PARTITION = "10" THEN
                    direction <= right_instruction.PARTITION;
                ELSIF right_instruction.PARTITION = "01" THEN
                    direction <= right_instruction.PARTITION;
                ELSE -- invalid instruction
                    ASSERT false REPORT "invalid partitioning" SEVERITY error;
                END IF;
            END IF;
        END IF;
    END PROCESS p_segment;

    p_direction : PROCESS (left_in, right_in, direction) IS
    BEGIN
        CASE direction IS
            WHEN "10" =>
                left_out  <= IDLE_BUS;
                right_out <= left_in;
            WHEN "01" =>
                left_out  <= right_in;
                right_out <= IDLE_BUS;
            WHEN OTHERS =>
                left_out  <= IDLE_BUS;
                right_out <= IDLE_BUS;
        END CASE;

    END PROCESS p_direction;

    bus_direction <= direction;

END ARCHITECTURE RTL;