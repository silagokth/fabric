-------------------------------------------------------
--! @file
--! @brief cell_config_swb
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
-- Title      : cell_config_swb
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : cell_config_swb.vhd
-- Author     : Mohammed Ali Shami
-- Company    : KTH
-- Created    : 2009
-- Last update: 2019-03-11
-- Platform   : SiLago
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Copyright (c) 2013 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                 Description
-- 2009         1.0     Mohammed Ali Shami
-- 2011         2.0     Fatemeh Ostad Ebrahim
-- 2014         3.0     Nasim Farahini
-------------------------------------------------------------------------------

--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
--                                                                         #
--This file is part of SiLago.                                             #
--                                                                         #
--    SiLago platform source code is distributed freely: you can           #
--    redistribute it and/or modify it under the terms of the GNU    	     #
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
-- This memory has been modified to support Self Reconfiguration. This memory is connected
-- to sequencers directly and can be programmed by the sequencers only. 
-- The memory protocol has been changed, now we do not have to specify the columns, since a
-- Sequencer in a column can program configuration memory in its on column only
-------------------------------------------------------------------------------
-------------------------
---NxM CrossBar 
---N Inputs connected to Fixed Wires
---M Outputs Connected to Ns through switches
-------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.top_consts_types_package.ALL;

ENTITY cell_config_swb IS
  PORT (
    rst_n           : IN  std_logic;
    clk             : IN  std_logic;
    swb_instr_in    : IN std_logic_vector(SWB_INSTR_PORT_SIZE - 1 DOWNTO 0);
    sel_r_int   : OUT s_bus_switchbox_ty;  --regs
    sel_r_ext_out		: OUT s_bus_switchbox_ty;
    data_from_other_row   : OUT std_logic_vector (5 downto 0)
    );

END cell_config_swb;

ARCHITECTURE beh OF cell_config_swb IS

 -- SIGNAL sel_r : s_bus_switchbox_ty;
  
BEGIN
  --sel_registers <= sel_r;
   
  p_sel_reg : PROCESS (clk, rst_n)
  BEGIN
    IF rst_n = '0' THEN
      data_from_other_row <= (OTHERS => '0');
      sel_r_int <= (OTHERS => (OTHERS => '1'));
      sel_r_ext_out <= (OTHERS => (OTHERS => '1'));
    ELSIF clk'event AND clk = '1' THEN
      IF(swb_instr_in(10) = '1') THEN
        data_from_other_row(to_integer(unsigned(swb_instr_in(2 DOWNTO 0))))<= swb_instr_in(3);
      	IF swb_instr_in(3) = '0' then 
        	sel_r_int(to_integer(unsigned(swb_instr_in(2 DOWNTO 0)))) <= swb_instr_in(9) & swb_instr_in(8) & swb_instr_in(7) & swb_instr_in(6 DOWNTO 4);
        ELSE 
        	sel_r_ext_out(to_integer(unsigned(swb_instr_in(2 DOWNTO 0)))) <= swb_instr_in(9) & swb_instr_in(8) & swb_instr_in(7) & swb_instr_in(6 DOWNTO 4);
      	END IF;
      END IF;
    END IF;
  END PROCESS p_sel_reg;

--swb_instr_in(2 DOWNTO 0)=v_index  
--swb_instr_in(3)= send_to_other_row
--swb_instr_in(6 DOWNTO 4)=hb_index  
--swb_instr_in(7)=from_port  
--swb_instr_in(8)=from_block 
--swb_instr_in(9)=src_addr_row 

END beh;
