-------------------------------------------------------
--! @file
--! @brief Switchbox module
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
-- Title      : switchbox, Contains 12 8-14X1 Tristated Multiplexers
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : switchbox.vhd
-- Author     : Muhammed Ali Shami  shami@kth.se
-- Company    : KTH
-- Created    : 2009-10-01
-- Last update: 2014-02-25
-- Platform   : 
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Copyright (c) 2013 
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  		        Description
-- 2009-10-01  1.0      Muhammed Ali Shami  Created
-- 2013-07-19  2.0      sadiq Hemani		    Updated
-- 2014-02-25  3.0      Nasim Farahini      Modified
-- 2019-03-11  3.1      Stathis Dimitrios   Added lic and comments
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

--! Include IEEE and work libraries
LIBRARY IEEE, work;
--! Use standard library
USE IEEE.std_logic_1164.ALL;
--! Use numeric library
USE ieee.numeric_std.ALL; 
--! Use the top constand package to include the constants and types
USE work.top_consts_types_package.ALL;

--! @brief Switchbox entity
--! @details The Switchbox is used to create the sliding window connections.
--! It gets configuration [select] signals from the sw_configuration of this DRRA cell
--! and the DRRA cell of the adjacent row in the same collumn. It outputs data to
--! its own cells DPU and register file, but also propagates data to the adjacent cell.
ENTITY switchbox IS
  GENERIC (BITWIDTH : integer := BITWIDTH;
           M        : integer);--:= MaxNrOfOutpNHopAway;);    -- # outps in NrOfHop range MaxNrOfOutpNHopAway=7)
  PORT (inputs_reg          : IN  h_bus_ty(0 TO M-1, 0 TO NR_OF_OUTP-1); --! Input from the register file
        inputs_dpu          : IN  h_bus_ty(0 TO M-1, 0 TO NR_OF_OUTP-1); --! Input from the DPU
        sel_r_int           : IN  s_bus_switchbox_ty; --! Select singals to control the swbox from the sw_config
        int_v_input_bus     : OUT v_bus_ty; --! Output data ports to the DPU and register file
        sel_r_ext_in        : IN  s_bus_switchbox_ty; --!  Select signals to route to the next row (comming form the next row)
        ext_v_input_bus_out : OUT v_bus_ty --! Output data to the next row
        );
END switchbox;

--! @brief Switchbox architecture
--! @details The Switchbox is used to configure and set the routing paths in the sliding window.
--! It selects the data that will be routed to the RF and DPU. It also routes data to the adjacent
--! cell in the same column.
ARCHITECTURE switchbox_architecture OF switchbox IS

BEGIN
  SwitchBox_Generate0 : FOR i IN 0 TO NR_OF_COL_INPS_ONE_CELL-1 GENERATE --NR_OF_COL_INPS_ONE_CELL-1 Generate
    Mux_Generate0 : ENTITY work.InputMux
      GENERIC MAP(BITWIDTH => BITWIDTH, M => M)
      PORT MAP(
        --clk    => clk,
        --rst_n  => rst_n,
        inputs_reg => inputs_reg,
        inputs_dpu => inputs_dpu,
        sel    => sel_r_int(i),
        output => int_v_input_bus(i));
  END GENERATE SwitchBox_Generate0;
  
    SwitchBox_Generate1 : FOR i IN 0 TO NR_OF_COL_INPS_ONE_CELL-1 GENERATE --NR_OF_COL_INPS_ONE_CELL-1 Generate
    Mux_Generate1 : ENTITY work.InputMux
      GENERIC MAP(BITWIDTH => BITWIDTH, M => M)
      PORT MAP(
        --clk    => clk,
        --rst_n  => rst_n,
        inputs_reg => inputs_reg,
        inputs_dpu => inputs_dpu,
        sel    => sel_r_ext_in(i),
        output => ext_v_input_bus_out(i));
  END GENERATE SwitchBox_Generate1;
END switchbox_architecture;
