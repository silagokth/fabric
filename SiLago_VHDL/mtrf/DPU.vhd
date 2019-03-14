-------------------------------------------------------
--! @file
--! @brief DPU
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
-- Title      : DPU Integer
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : DPU.vhd
-- Author     : Hojat Khoshroshahi <hojatk@s1392>
-- Company    : KTH
-- Created    : 2013-08-27
-- Last update: 2019-03-11
-- Platform   : SiLago
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Copyright (c) 2013 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author            Description
-- 2013-08-27  1.0      Hojatk            Created
-- 2013-10-20  2.0      Nasim Farahini
-- 2014-02-26  3.0      Nasim Farahini
-- 2015-02-22  4.0      Hassan Sohofi     Updating the branch instruction.  
-- 2016-02-07  5.0      Arun Jayabalan    Fixed Point and Saturation Logic 
-- 2016-04-26  6.0      Arun Jayabalan    Arithmetic shift     
-- 2019-03-11  7.0      Dimitrios Stathis Fix bugs
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

--! IEEE and work Library
LIBRARY ieee, work;
--! Use standard library
USE ieee.std_logic_1164.ALL;
--! Use numeric standard library for arithmetic operations
USE ieee.numeric_std.ALL;
--! Use signed library (tread all logic_vectors as signed numbers)
USE IEEE.std_logic_signed.ALL; -- @TODO remove signed library, only the numeric std library should be used
--! Use the top constand and type definition package
USE work.top_consts_types_package.ALL;

--! @brief This is the main data processing unit of the DRRA
--! @detail The DPU is is configured by the sequencer with the 
--! needed computational mode. It has 4 input ports of 16-bit and
--! 2 outputs. 
ENTITY DPU IS
  PORT (
        clk               : IN std_logic;
        rst_n             : IN std_logic;
        dpu_ctrl_out_0    : IN std_logic_vector (DPU_CTRL_OUT_WIDTH-1 DOWNTO 0);
        dpu_ctrl_out_1    : IN std_logic_vector (DPU_CTRL_OUT_WIDTH-1 DOWNTO 0);

        dpu_in_0          : IN signed (DPU_IN_WIDTH-1 DOWNTO 0);  --signed
        dpu_in_1          : IN signed (DPU_IN_WIDTH-1 DOWNTO 0);  --signed
        dpu_in_2          : IN signed (DPU_IN_WIDTH-1 DOWNTO 0);  --signed
        dpu_in_3          : IN signed (DPU_IN_WIDTH-1 DOWNTO 0);  --signed

        dpu_mode_cfg      : IN std_logic_vector (DPU_MODE_CFG_WIDTH-1 DOWNTO 0);
        dpu_acc_clear_rst : IN std_logic;
        dpu_acc_clear     : IN std_logic_vector (DPU_ACC_CLEAR_WIDTH-1 DOWNTO 0);

        dpu_process_inout : IN std_logic_vector (DPU_PROCESS_INOUT_WIDTH-1 DOWNTO 0) ;

        dpu_out_0_left    : OUT signed (DPU_OUT_WIDTH-1 DOWNTO 0);  --signed
        dpu_out_0_right   : OUT signed (DPU_OUT_WIDTH-1 DOWNTO 0);  --signed

        dpu_out_1_left    : OUT signed (DPU_OUT_WIDTH-1 DOWNTO 0);  --signed
        dpu_out_1_right   : OUT signed (DPU_OUT_WIDTH-1 DOWNTO 0);  --signed

        dpu_sat_ctrl      : IN  std_logic_vector (DPU_SAT_CTRL_WIDTH-1 DOWNTO 0); 
        seq_cond_status   : OUT std_logic_vector (SEQ_COND_STATUS_WIDTH-1 DOWNTO 0)
    );
END DPU;
--! @brief TArchitecture of DPU
--! @detail The DPU operates in different MODES, and it can do Q15 or integer arithmetics
ARCHITECTURE behave OF DPU IS
  
  SIGNAL acc_signl        : signed(ACC_WIDTH-1 DOWNTO 0);     -- Accumulator 
  SIGNAL min_tmp_reg      : signed(DPU_IN_WIDTH-1 DOWNTO 0) ; -- registered min_tmp,--signed
  SIGNAL max_tmp_reg      : signed(DPU_IN_WIDTH-1 DOWNTO 0) ; -- registered max_tmp,--signed
  SIGNAL max_tmp          : signed(DPU_IN_WIDTH-1 DOWNTO 0);  -- max_tmp,--signed
  SIGNAL min_tmp          : signed(DPU_IN_WIDTH-1 DOWNTO 0);  -- min_tmp,--signed
  SIGNAL in_0_signl       : signed(DPU_IN_WIDTH-1 DOWNTO 0);  -- input 0,--signed
  SIGNAL in_1_signl       : signed(DPU_IN_WIDTH-1 DOWNTO 0);  -- input 1,--signed
  SIGNAL in_2_signl       : signed(DPU_IN_WIDTH-1 DOWNTO 0);  -- input 2,--signed
  SIGNAL in_3_signl       : signed(DPU_IN_WIDTH-1 DOWNTO 0);  -- input 3,--signed
  SIGNAL in_3_reg         : signed(DPU_IN_WIDTH-1 DOWNTO 0);  -- input 3 registered--signed
  SIGNAL in_0_reg         : signed(DPU_IN_WIDTH-1 DOWNTO 0);  -- input 0 registered--signed
  SIGNAL mul_in0          : signed(DPU_IN_WIDTH-1 DOWNTO 0);  -- multiplier input 0
  SIGNAL mul_in1          : signed(DPU_IN_WIDTH-1 DOWNTO 0);  -- multiplier input 1
  SIGNAL mul_out          : signed(2*DPU_OUT_WIDTH-1 DOWNTO 0);  -- multiplier output
  SIGNAL count_check      : signed(DPU_OUT_WIDTH-1 DOWNTO 0);  -- counter flag when it reaches the max value
  SIGNAL count_en         : std_logic;                         -- enable for counting mode
  SIGNAL count            : signed(DPU_OUT_WIDTH-1 DOWNTO 0);  -- counter register
  SIGNAL count_rst        : bit;                               -- control bit to clear count signal during back to back counter operation 
  SIGNAL comp_0_signl     : signed(DPU_OUT_WIDTH-1 DOWNTO 0);  --comparator signals
  SIGNAL comp_1_signl     : signed(DPU_OUT_WIDTH-1 DOWNTO 0);
  SIGNAL acc_clr_cnt_reg  : std_logic_vector (DPU_ACC_CLEAR_WIDTH DOWNTO 0);
  SIGNAL acc_clr,  acc_clr_cnt_en, acc_clr_reg, minmax_clr  : std_logic; 
  SIGNAL comp_st          : std_logic_vector(1 DOWNTO 0);

  SIGNAL out_0_signl_temp : signed(2*DPU_OUT_WIDTH+1 DOWNTO 0);
  SIGNAL out_1_signl_temp : signed(2*DPU_OUT_WIDTH+1 DOWNTO 0);

  SIGNAL out_0_signl      : signed(DPU_OUT_WIDTH-1 DOWNTO 0);
  SIGNAL out_1_signl      : signed(DPU_OUT_WIDTH-1 DOWNTO 0);
  SIGNAL out_0_reg        : signed(DPU_OUT_WIDTH-1 DOWNTO 0);     -- out0 register
  SIGNAL out_1_reg        : signed(DPU_OUT_WIDTH-1 DOWNTO 0);     -- out1 register

  -- dpu_sat_ctrl[0]= 0 -> Integer       ; dpu_sat_ctrl[0]= 1 -> fixedpoint
  -- dpu_sat_ctrl[1]= 0 -> No saturation ; dpu_sat_ctrl[1]= 1 -> saturation


BEGIN
  --seq_cond_status <= (OTHERS => '0');


  in_0_signl <= -dpu_in_0 when dpu_process_inout = NEGATE_IN0 else dpu_in_0;  --negates in_0 if dpu_process_inout= "01"
  in_1_signl <= -dpu_in_1 when dpu_process_inout = NEGATE_IN1 else dpu_in_1;  --negates in_1 if dpu_process_inout= "10"
  in_2_signl <= dpu_in_2;
  in_3_signl <= dpu_in_3;

--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§  
-- purpose: making output ports registered
-- type   : clocked
-- inputs : clk,rst_n
-- outputs: in_0_reg and in_3_reg

  in_3_registered : PROCESS (clk, rst_n)
  BEGIN  -- process in_3_registered
    IF rst_n = '0' THEN                 -- asynchronous reset (active low) 
      in_0_reg    <= (OTHERS => '0');
      in_3_reg    <= (OTHERS => '0');      
    ELSIF clk'event AND clk = '1' THEN  -- rising clock edge
      in_0_reg    <= in_0_signl;
      in_3_reg    <= in_3_signl;
    END IF;
  END PROCESS in_3_registered;

  acc_clr     <= '1' when  acc_clr_cnt_reg = dpu_acc_clear   or dpu_acc_clear_rst='1'  else '0';
  minmax_clr  <= '1' when  dpu_acc_clear_rst='1'  else '0';

  acc_clr_cnt_process : PROCESS (clk, rst_n)
  BEGIN  
    IF rst_n = '0' THEN           
      acc_clr_cnt_reg <= (others => '0');
    ELSIF clk'event AND clk = '1' THEN  
      IF acc_clr_cnt_en='1' and acc_clr_reg = '0'  THEN
     		acc_clr_cnt_reg <= acc_clr_cnt_reg+1;
      ELSE
    		acc_clr_cnt_reg <= (OTHERS =>'0');
      END IF;
    END IF;
  END PROCESS acc_clr_cnt_process;
  
  acc_clr_reg_process : PROCESS (clk, rst_n)
  BEGIN  -- process acc_clr_reg_process
    IF rst_n = '0' THEN                 -- asynchronous reset (active low)
      acc_clr_reg <= '0';
    ELSIF clk'event AND clk = '1' THEN  -- rising clock edge
      acc_clr_reg <= acc_clr;
    END IF;
  END PROCESS acc_clr_reg_process;

--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
-- purpose: to detect overflow 
-- type   : combinational
-- inputs : out_0_signl_temp, out_1_signl_temp
-- outputs: out_0_signl, out_1_signal
-- seq_cond_status values: 00: otherwise, 01: greater is true, 10: equal is true.
  
  overflow_process : PROCESS ( out_0_signl_temp, out_1_signl_temp )

  VARIABLE add_overflow : std_logic_vector(1 DOWNTO 0); --00 - No overflow, 01 - overflow, 10 - underflow
  VARIABLE sub_overflow : std_logic_vector(1 DOWNTO 0); --00 - No overflow, 01 - overflow, 10 - underflow

  BEGIN

    IF dpu_sat_ctrl(1) = '1' THEN -- Saturation

      IF out_0_signl_temp >  to_signed(32767,out_0_signl_temp'length)  THEN
          out_0_signl   <= "0111111111111111";
          add_overflow  := "10";
      ELSIF out_0_signl_temp <  to_signed(-32768, out_0_signl_temp'length)  THEN
          out_0_signl   <= "1000000000000000";
          add_overflow  := "01";
      ELSE 
        out_0_signl <= out_0_signl_temp(DPU_OUT_WIDTH-1 downto 0);
      END IF;

      IF out_1_signl_temp >  to_signed(32767, out_1_signl_temp'length)  THEN
          out_1_signl   <= "0111111111111111";
          add_overflow  := "10";
      ELSIF out_1_signl_temp <  to_signed(-32768, out_1_signl_temp'length)  THEN
          out_1_signl   <= "1000000000000000";
          add_overflow  := "01";
      ELSE 
        out_1_signl <= out_1_signl_temp(DPU_OUT_WIDTH-1 downto 0);
      END IF;
      
      -- IF add_overflow /= "00" or sub_overflow /= "00" THEN
      --   seq_st  <=  "11";
      -- ELSE 
      --   seq_st  <=  "00";
      -- END IF;  

    ELSE
        out_0_signl <= out_0_signl_temp(DPU_OUT_WIDTH-1 downto 0);
        out_1_signl <= out_1_signl_temp(DPU_OUT_WIDTH-1 downto 0);
    END IF;

  END PROCESS overflow_process;

--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

dpu_mode_process_ov : PROCESS ( acc_signl, comp_0_signl, comp_1_signl, 
                                dpu_mode_cfg , in_0_signl, in_1_signl, 
                                in_2_signl, in_3_signl, mul_out, max_tmp, 
                                min_tmp, dpu_acc_clear, dpu_sat_ctrl)

  VARIABLE mul_out_var : signed(2*DPU_OUT_WIDTH+1 DOWNTO 0);
  VARIABLE acc_var : signed(ACC_WIDTH-1 DOWNTO 0);
  VARIABLE in_0_var,in_1_var : signed(2*DPU_OUT_WIDTH+1 DOWNTO 0);
  VARIABLE in_2_var,in_3_var : signed(2*DPU_OUT_WIDTH+1 DOWNTO 0);
  VARIABLE comp_0_var,comp_1_var : signed(2*DPU_OUT_WIDTH+1 DOWNTO 0);
  VARIABLE max_tmp_var,min_tmp_var: signed(2*DPU_OUT_WIDTH+1 DOWNTO 0);
  VARIABLE in_0_var_abs, in_1_var_abs : signed(DPU_OUT_WIDTH DOWNTO 0);
  VARIABLE abs_temp_1 : signed(DPU_OUT_WIDTH DOWNTO 0);
  VARIABLE const_value_var : signed(2*DPU_OUT_WIDTH+1 DOWNTO 0);
  VARIABLE const_init_var : std_logic_vector(DPU_OUT_WIDTH-DPU_ACC_CLEAR_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
BEGIN
  acc_clr_cnt_en      <= '0';

  acc_var  := resize(acc_signl, acc_var'length );
  in_0_var := resize(in_0_signl, in_0_var'length );
  in_1_var := resize(in_1_signl, in_1_var'length );
  in_2_var := resize(in_2_signl, in_2_var'length );
  in_3_var := resize(in_3_signl, in_3_var'length );
  mul_out_var := resize( mul_out, mul_out_var'length);
  comp_0_var  := resize(comp_0_signl, comp_0_var'length);
  comp_1_var  := resize(comp_1_signl, comp_1_var'length);
  max_tmp_var := resize( max_tmp, max_tmp_var'length );
  min_tmp_var := resize( min_tmp, min_tmp_var'length );
  in_0_var_abs := resize(in_0_signl, abs_temp_1'length );
  in_1_var_abs := resize(in_1_signl, abs_temp_1'length );
  abs_temp_1 := (others => '0');
  const_value_var := (others => '0');

  CASE CONV_INTEGER(dpu_mode_cfg) IS

        WHEN MODE_1   =>                          -- out_0 = [(in_0 + in_1) *in_2] + out_0_reg
                                                  -- out_1 = [(in_0 + in_1) *in_2] 
            acc_clr_cnt_en    <= '1';
            out_0_signl_temp  <= mul_out_var + acc_var ;
            out_1_signl_temp  <= mul_out_var;

        WHEN MODE_2   =>                          -- out_0 = (in_0 * in_2) + out_0_reg
                                                  -- out_1 = (in_0 * in_2)
            acc_clr_cnt_en    <= '1';
            out_0_signl_temp  <= mul_out_var + acc_var ;
            out_1_signl_temp  <= mul_out_var ;

        -- In this MAC operation, the accumulator is initialized with in_3_signl
        WHEN MODE_3   =>                          -- out_0 = (in_0 * in_2) + (out_0_reg/in_3_signl)
                                                  -- out_1 = (in_0 * in_2)
            acc_clr_cnt_en    <= '1';
            out_0_signl_temp  <= mul_out_var + acc_var ;
            out_1_signl_temp  <= mul_out_var ;

        WHEN MODE_4   =>                          -- out_0 = (in_0 * in_2) + in_3
                                                  -- out_1 = in_1 - (in_0 * in_2)
            out_0_signl_temp  <= in_3_var + mul_out_var ;
            out_1_signl_temp  <= in_1_var - mul_out_var ;

        WHEN MODE_5   =>                          -- out_0 = (in_0_reg * in_2) + in_3
                                                  -- out_1 = in_1 - (in_0_reg * in_2)
            out_0_signl_temp  <= in_3_var + mul_out_var;
            out_1_signl_temp  <= in_1_var - mul_out_var ;

        WHEN MODE_6   =>                          -- out0 = max(in0, max_tmp_reg)
                                                  -- out1 = min(in0, min_tmp_reg)
            acc_clr_cnt_en    <= '1';
            out_0_signl_temp  <= max_tmp_var;
            out_1_signl_temp  <= min_tmp_var;

        WHEN MODE_7   =>                         -- absolute function out0 = SUM(abs(in0-in1))
                                                 -- out1 = abs(in0-in1)
            acc_clr_cnt_en    <= '1'; 
            abs_temp_1 :=  in_0_var_abs - in_1_var_abs ;

            IF dpu_process_inout = ABSOLUTE_OUT THEN
              abs_temp_1 := abs(abs_temp_1);
            END IF;

            out_0_signl_temp  <= acc_var + abs_temp_1;
            out_1_signl_temp  <= (others => '0')  ;

        -- WHEN mode_8 => in use for counter operation  
           

        WHEN MODE_9   =>                          -- out_0 = (in_0 << in_1) arithmetic left shift
                                                  -- out_1 = (in_0 >> in_1) arithmetic right shift
            out_0_signl_temp  <=  SHIFT_LEFT(in_0_var, to_integer(in_1_var));
            out_1_signl_temp  <=  SHIFT_RIGHT(in_0_var, to_integer(in_1_var));

        WHEN MODE_10  =>                          -- out_0 = (in_2 + in_3) 
                                                  -- out_1 = (in_0 - in_1) 
            out_0_signl_temp  <= in_2_var  + in_3_var;
            out_1_signl_temp  <= in_0_var  - in_1_var;

        WHEN MODE_11  =>                          -- in0 > in1 => out0 = 111...11
                                                  -- in2 > in3 => out1 = 111...11
            out_0_signl_temp  <=  comp_0_var;
            out_1_signl_temp  <=  comp_1_var;

        -- Here dpu_acc_clear is used as a constant value
        WHEN MODE_12 =>                           -- out_0 = constant
                                                  -- out_1 = in0 + constant
            
            IF dpu_sat_ctrl(0) = '0' THEN -- integer value
              const_value_var := to_signed(CONV_INTEGER(dpu_acc_clear), const_value_var'length);
            ELSE -- fixed-point value
              const_value_var := to_signed(CONV_INTEGER(dpu_acc_clear&const_init_var), const_value_var'length);
            END IF;

            out_0_signl_temp <= const_value_var;
            out_1_signl_temp <= in_0_var + const_value_var;

        WHEN MODE_13  =>                        -- comparison with constant

        WHEN OTHERS =>
            out_0_signl_temp  <= (OTHERS => '0');
            out_1_signl_temp  <= (OTHERS => '0');    

  END CASE;

END PROCESS  dpu_mode_process_ov;

--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  -- purpose: to create comparator
  -- type   : combinational
  -- inputs : dpu_cfg_mode
  -- outputs: comp_0_signl,comp_1_signl, seq_cond_status
  -- seq_cond_status values: 00: otherwise, 01: greater is true, 10: equal is true.
  comparator_process : PROCESS (dpu_mode_cfg, in_0_signl, in_1_signl,
                                in_2_signl, in_3_signl, dpu_acc_clear, dpu_sat_ctrl)
    VARIABLE const_value_var : signed(2*DPU_OUT_WIDTH+1 DOWNTO 0);
    VARIABLE const_init_var : std_logic_vector(DPU_OUT_WIDTH-DPU_ACC_CLEAR_WIDTH-1 DOWNTO 0) := (OTHERS => '0');
  BEGIN  -- process comparator_process
    comp_st <= "00";
    comp_0_signl <= (OTHERS => '0');
    comp_1_signl <= (OTHERS => '0');
    const_value_var := (OTHERS => '0');

    IF CONV_INTEGER(dpu_mode_cfg) = MODE_11 THEN
      
      IF in_0_signl > in_1_signl THEN
        comp_0_signl <= (OTHERS => '1');
        comp_st <= "01";
      ELSIF in_0_signl = in_1_signl THEN
        comp_st <= "10";
      END IF;

      IF in_2_signl > in_3_signl THEN
        comp_1_signl <= (OTHERS => '1');
      END IF;

    ELSIF CONV_INTEGER(dpu_mode_cfg) = MODE_13 THEN
      
      IF dpu_sat_ctrl(0) = '0' THEN -- integer value
        const_value_var := to_signed(CONV_INTEGER(dpu_acc_clear), const_value_var'length);
      ELSE -- fixed-point value
        const_value_var := to_signed(CONV_INTEGER(dpu_acc_clear&const_init_var), const_value_var'length);
      END IF;

      IF in_0_signl > const_value_var THEN
        comp_st <= "01";
      ELSIF in_0_signl = const_value_var THEN
        comp_st <= "10";
      END IF;

    END IF;
  END PROCESS comparator_process;

  seq_cond_status <= comp_st ;

--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  maxmin_process : PROCESS (dpu_mode_cfg, in_0_reg,
                            max_tmp_reg, min_tmp_reg, minmax_clr)
  BEGIN
    max_tmp <= max_tmp_reg;
    min_tmp <= min_tmp_reg;

    IF CONV_INTEGER(dpu_mode_cfg) = MODE_6 and minmax_clr = '0' THEN
      IF max_tmp_reg < in_0_reg THEN
        max_tmp <= in_0_reg;
      END IF;    
      IF min_tmp_reg > in_0_reg THEN
        min_tmp <= in_0_reg;
      END IF;
    END IF;

  END PROCESS maxmin_process;

--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  -- purpose: sources of accumulator signal according to dpu modes
  -- type   : combinational
  -- inputs : 
  -- outputs: acc_signl
  accumulator_process : PROCESS (acc_clr, dpu_mode_cfg, 
                                 out_0_reg)  
  BEGIN  -- process accumulator_process
    CASE CONV_INTEGER(dpu_mode_cfg) IS
      WHEN mode_1 | mode_2 | mode_7 =>
        IF acc_clr = '1' THEN
          acc_signl <= (OTHERS => '0');
        ELSE 
          acc_signl <= resize(out_0_reg, acc_signl'length);
        END IF;

      WHEN mode_3 =>
        IF acc_clr_reg = '1' THEN
          acc_signl <= resize(in_3_signl, acc_signl'length);
        ELSE 
          acc_signl <= resize(out_0_reg, acc_signl'length);
        END IF;
  
      WHEN OTHERS =>
        acc_signl <= (OTHERS => '0');
    END CASE;

  END PROCESS accumulator_process;

--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  -- purpose: connecting inputs of multiplier
  -- type   : combinational
  -- inputs : in_0_signl,in_1_signl,in_2_signl
  -- outputs: mul_in0,mul_in1
  multiplication_process : PROCESS (dpu_mode_cfg, in_0_signl, in_1_signl, in_2_signl,in_0_reg)

   VARIABLE ADD_TEMP : signed(DPU_OUT_WIDTH DOWNTO 0);

  BEGIN  -- process multiplication
    CASE CONV_INTEGER(dpu_mode_cfg) IS
      WHEN mode_1 => 
      
		     ADD_TEMP := resize(in_0_signl, ADD_TEMP'length) + resize(in_1_signl, ADD_TEMP'length);
         mul_in0 <= ADD_TEMP(DPU_OUT_WIDTH-1 downto 0);
         mul_in1 <= in_2_signl;
			    
         -- Mode_1 mul_in0 = in_0_signl+in_1_signl can possibly have OV
         -- saturate mul_in0 accordingly if there is an OV
		     IF ADD_TEMP(DPU_OUT_WIDTH) /= ADD_TEMP(DPU_OUT_WIDTH-1) THEN
            IF ADD_TEMP(DPU_OUT_WIDTH) = '1' THEN
                mul_in0 <= "1000000000000000";
            ELSE
                mul_in0 <= "0111111111111111";
           	END IF;
		     END IF;         

      WHEN mode_2 | mode_3 |  mode_4    => mul_in0 <= in_0_signl;
                                           mul_in1 <= in_2_signl;

      WHEN mode_5  => mul_in0 <= in_0_reg;
                      mul_in1 <= in_2_signl; 
                                                         
      WHEN OTHERS => mul_in0 <= (OTHERS => '0');
                     mul_in1 <= (OTHERS => '0');
    END CASE;
  END PROCESS multiplication_process;

--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  -- purpose: multiplier internal component
  -- type   : sequential
  -- inputs : clk, rst_n, mul_in0,mul_in1
  -- outputs: mul_out
  mul_clocked : PROCESS (clk, rst_n)
  VARIABLE mul_out_temp : signed(2*DPU_OUT_WIDTH-1 DOWNTO 0);

  BEGIN  -- process mul_clocked

    IF rst_n = '0' THEN                  -- asynchronous reset (active low)
      mul_out       <= (OTHERS => '0');
      mul_out_temp  := (OTHERS => '0');
    ELSIF clk'event AND clk = '1' THEN   -- rising clock edge 
	    mul_out_temp := mul_in0 * mul_in1;

      IF dpu_process_inout = 3 THEN
          mul_out_temp  := abs(mul_out_temp);
      END IF;

      
	    IF dpu_sat_ctrl(0) = '1' THEN -- FIXED POINT OPERATION (Q15 format - 1 sign bit + 15 fixed point -1.000 to 0.99996948242)
          mul_out          <= resize(mul_out_temp(2*DPU_OUT_WIDTH-2 DOWNTO 15), mul_out'length);
      ELSE
    	    mul_out          <= mul_out_temp;
      END IF; -- FIXED POINT CONTROL
    END IF;  -- ELSEIF CLK'EVENT
  END PROCESS mul_clocked;

--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  
  -- purpose: making status ports registered
  -- type   : clocked
  -- inputs : clk,rst_n
  -- outputs: seq_st_reg
  -- seq_status_reg: PROCESS(rst_n, clk)
  -- Enable it if seq_cond_status to be registered
  --   BEGIN
  --     IF rst_n = '0' THEN
  --         seq_st_reg <= (OTHERS => '0');
  --     ELSIF rising_edge(clk) THEN
  --         seq_st_reg <= seq_st;      
  --     END IF;
  -- END PROCESS  seq_status_reg;
  
  -- seq_cond_status <= seq_st_reg ;

--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  -- purpose: enableing the counter mode
  -- type   : combinational
  -- inputs : dpu_cfg_mode
  -- outputs: count_en
  counter_enable : PROCESS (dpu_mode_cfg)
  BEGIN  -- process counter_process
    CASE CONV_INTEGER(dpu_mode_cfg) IS
      WHEN mode_8 =>
        count_en <= '1';
      WHEN OTHERS =>
        count_en <= '0';
    END CASE;
  END PROCESS counter_enable;

--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  -- purpose: creating internal counter
  -- type   : clocked
  -- inputs : clk, rst_n, count_en
  -- outputs: count_out
  counter_process : PROCESS (clk, rst_n)
  BEGIN  -- process counter_process
    IF rst_n = '0' THEN                 -- asynchronous reset (active low)
      count <= (OTHERS => '0');

    ELSIF clk'event AND clk = '1' THEN  -- rising clock edge
      IF count_en = '0' or count_rst = '1' THEN
        count <= (OTHERS => '0'); 
      ELSE
  --      CASE dpu_in_0 IS
  --        WHEN UP_COUNT =>
  --         count <= count + 1;
  --        WHEN RESET_COUNT =>
  --          count <= (OTHERS => '0');
  --        WHEN SET_COUNT =>
  --          count <= in_1_signl;
  --        WHEN DOWN_COUNT =>
  --          count <= count - 1;
  --        WHEN OTHERS => NULL;
  --      END CASE;
  IF dpu_in_0 = UP_COUNT THEN
            count <= count + 1;
        ELSIF dpu_in_0 = RESET_COUNT THEN
            count <= (OTHERS => '0');
        ELSIF dpu_in_0 = SET_COUNT THEN
            count <= in_1_signl;
        ELSIF dpu_in_0 =  DOWN_COUNT THEN
            count <= count - 1;
        ELSE count <= (OTHERS => '0');
        END IF;
      END IF;
    END IF;
  END PROCESS counter_process;

--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  -- purpose: activates the counter flag when it reaches the required value
  -- type   : combinational
  -- inputs : count_en,in_0_signl
  -- outputs: count_check
  count_check_process : PROCESS (count, count_en, in_0_signl, in_1_signl)
  BEGIN  -- process count_check_process
    IF count_en = '1' THEN
    --  CASE in_0_signl IS
    --    WHEN UP_COUNT =>
    --     IF count = in_1_signl THEN
    --        count_check <= (OTHERS => '1');
    --      ELSE
    --        count_check <= (OTHERS => '0');
    --      END IF;
    --    WHEN DOWN_COUNT =>
    --      IF count = signed(to_unsigned(0, count'length)) THEN
    --        count_check <= (OTHERS => '1');
    --      ELSE
    --        count_check <= (OTHERS => '0');
    --      END IF;
    --    WHEN OTHERS => count_check <= (OTHERS => '0');
    --  END CASE;

--UP_COUNT    = 7,  DOWN_COUNT  = 4,  SET_COUNT   = 5,  RESET_COUNT   = 6,  STOP_COUNT  = 8 

       IF in_0_signl =  UP_COUNT THEN

          IF count = in_1_signl THEN
            count_check <= (OTHERS => '1');
            count_rst   <= '1';
          ELSE
            count_check <= (OTHERS => '0');
            count_rst <= '0';
          END IF;
          
      ELSIF in_0_signl =  DOWN_COUNT THEN
          IF count = signed(to_unsigned(0, count'length)) THEN
            count_check <= (OTHERS => '1');
            count_rst   <= '1';
          ELSE
            count_check <= (OTHERS => '0');
            count_rst   <= '0';
          END IF;
      ELSE 
        count_check <= (OTHERS => '0');
        count_rst   <= '1';
      END IF;
  
    ELSE
      count_check <= (OTHERS => '0');
      count_rst   <= '0';
    END IF;
  END PROCESS count_check_process;

--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  -- purpose: making output ports registered
  --          Output registers are also being used for keeping min/max current values.
  -- type   : clocked
  -- inputs : clk,rst_n
  -- outputs: out_0_reg and out_1_reg
  dpu_reg_out : PROCESS (clk, rst_n)
  BEGIN  -- process dpu_reg_out
    IF rst_n = '0' THEN
      out_0_reg <= (OTHERS => '0');
      out_1_reg <= (OTHERS => '0');
    ELSIF rising_edge(clk) THEN
      IF CONV_INTEGER(dpu_mode_cfg) = MODE_6 THEN
        IF minmax_clr = '0' THEN
          out_0_reg   <= max_tmp;
          out_1_reg   <= min_tmp; 
        ELSE
          out_0_reg   <= (DPU_IN_WIDTH-1 => '1', OTHERS => '0');
          out_1_reg   <= (DPU_IN_WIDTH-1 => '0', OTHERS => '1');
        END IF;
      ELSE
        out_0_reg <= out_0_signl;
        out_1_reg <= out_1_signl;
      END IF;
    END IF;
  END PROCESS dpu_reg_out;

  max_tmp_reg <= out_0_reg;
  min_tmp_reg <= out_1_reg;

--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
    
  -- purpose: switching the out2 according to control right or left or both
  -- type   : combinational
  -- inputs : out_0_reg,dpu_ctrl_out_0
  -- outputs: dpu_out_0_left/right
  demux_dpu_out0 : PROCESS (count_check, dpu_ctrl_out_0, dpu_mode_cfg, out_0_reg)
  
  BEGIN  
    dpu_out_0_right <= (OTHERS => '0');
    dpu_out_0_left  <= (OTHERS => '0');
    
     CASE CONV_INTEGER(dpu_mode_cfg) IS
       WHEN mode_8 =>
         if dpu_ctrl_out_0(0)='1' then 
         	dpu_out_0_right <= count_check;
         end if;
         if dpu_ctrl_out_0(1)='1' then 
         	dpu_out_0_left  <= count_check;
         end if;
       WHEN OTHERS =>
        if dpu_ctrl_out_0(0)='1' then 
        	dpu_out_0_right <= out_0_reg(dpu_out_width-1 DOWNTO 0);
        end if ;
        if dpu_ctrl_out_0(1)='1' then 
        	dpu_out_0_left  <= out_0_reg(dpu_out_width-1 DOWNTO 0);
        end if;
        
    END CASE;

  END PROCESS demux_dpu_out0;

--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  -- purpose: switching the outputs according to control right or left or both
  -- type   : combinational
  -- inputs : out_1_reg,dpu_ctrl_out_1
  -- outputs: dpu_out_1_left/right
  demux_dpu_out1 : PROCESS ( count, dpu_ctrl_out_1, dpu_mode_cfg, out_1_reg )
  BEGIN  
    dpu_out_1_right <= (OTHERS => '0');
    dpu_out_1_left <= (OTHERS => '0');
    
    CASE CONV_INTEGER(dpu_mode_cfg) IS
      WHEN mode_8 =>
      if dpu_ctrl_out_1(0)='1' then 
        dpu_out_1_right <= count;
      end if;
      if dpu_ctrl_out_1(1)='1' then 
        dpu_out_1_left  <= count;
      end if;
      WHEN OTHERS =>

        if dpu_ctrl_out_1(0)='1' then 
          dpu_out_1_right <= out_1_reg(dpu_out_width-1 DOWNTO 0);
        end if ;
        if dpu_ctrl_out_1(1)='1' then 
          dpu_out_1_left  <= out_1_reg(dpu_out_width-1 DOWNTO 0);
        end if;

    END CASE;
--      when others =>
--        dpu_out_1_left  <= (others => '0');
--        dpu_out_1_right <= (others => '0');
--    end case;
  END PROCESS demux_dpu_out1;

END behave;
