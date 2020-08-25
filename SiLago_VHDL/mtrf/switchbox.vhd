---------------- Copyright (c) notice 2018 ------------------------------------
--
-- The VHDL code, the logic and concepts described in this file constitute
-- the intellectual property of the authors listed below, who are affiliated
-- to KTH(Kungliga Tekniska Högskolan), School of ICT, Kista.
-- Any unauthorised use, copy or distribution is strictly prohibited.
-- Any authorised use, copy or distribution should carry this copyright notice
-- unaltered.
-------------------------------------------------------------------------------
-- Title      : switchbox, Contains 12 8-14X1 Tristated Multiplexers
-- Project    : MTRF Fabric
-------------------------------------------------------------------------------
-- File       : switchbox.vhd
-- Author     : Muhammed Ali Shami  shami@kth.se
-- Company    : KTH
-- Created    : 2009-10-01
-- Last update: 2014-02-25
-- Platform   : 
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Description: <cursor>
-------------------------------------------------------------------------------
-- Copyright (c) 2013 
-------------------------------------------------------------------------------
-- Contact    : Nasim Farahini <farahini@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  		    Description
-- 2009-10-01  1.0      Muhammed Ali Shami  Created
-- 2013-07-19  2.0      sadiq Hemani		Updated
-- 2014-02-25  3.0      Nasim Farahini      Modified
-------------------------------------------------------------------------------

LIBRARY IEEE, work;
USE IEEE.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
--USE ieee.std_logic_unsigned.ALL;
USE work.top_consts_types_package.ALL;

ENTITY switchbox IS
    GENERIC
    (
        BITWIDTH : INTEGER := BITWIDTH;
        M        : INTEGER);--:= MaxNrOfOutpNHopAway;);    -- # outps in NrOfHop range MaxNrOfOutpNHopAway=7)
    PORT
    (
        inputs_reg          : IN h_bus_ty(0 TO M - 1, 0 TO NR_OF_OUTP - 1);
        inputs_dpu          : IN h_bus_ty(0 TO M - 1, 0 TO NR_OF_OUTP - 1);
        sel_r_int           : IN s_bus_switchbox_ty;
        int_v_input_bus     : OUT v_bus_ty;
        sel_r_ext_in        : IN s_bus_switchbox_ty;
        ext_v_input_bus_out : OUT v_bus_ty);
END switchbox;

ARCHITECTURE switchbox_architecture OF switchbox IS

BEGIN
    SwitchBox_Generate0 : FOR i IN 0 TO NR_OF_COL_INPS_ONE_CELL - 1 GENERATE --NR_OF_COL_INPS_ONE_CELL-1 Generate
        Mux_Generate0 : ENTITY work.InputMux
            GENERIC
            MAP(BITWIDTH => BITWIDTH, M => M)
            PORT MAP
            (
                --clk    => clk,
                --rst_n  => rst_n,
                inputs_reg => inputs_reg,
                inputs_dpu => inputs_dpu,
                sel        => sel_r_int(i),
                output     => int_v_input_bus(i));
    END GENERATE SwitchBox_Generate0;

    SwitchBox_Generate1 : FOR i IN 0 TO NR_OF_COL_INPS_ONE_CELL - 1 GENERATE --NR_OF_COL_INPS_ONE_CELL-1 Generate
        Mux_Generate1 : ENTITY work.InputMux
            GENERIC
            MAP(BITWIDTH => BITWIDTH, M => M)
            PORT
            MAP(
            --clk    => clk,
            --rst_n  => rst_n,
            inputs_reg => inputs_reg,
            inputs_dpu => inputs_dpu,
            sel        => sel_r_ext_in(i),
            output     => ext_v_input_bus_out(i));
    END GENERATE SwitchBox_Generate1;
END switchbox_architecture;