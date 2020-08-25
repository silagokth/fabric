-------------------------------------------------------
--! @file sequencer.vhd
--! @brief UnitX
--! @details 
--! @author Sadiq Hemani
--! @version 1.0
--! @date 2013 08 14
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
-- File       : sequencer.vhd
-- Author     : Sadiq Hemani
-- Company    : KTH
-- Created    : 2013 08 14
-- Last update: 2015 02 22
-- Platform   : SiLago
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Copyright (c) 2013
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Rev 1: Sadiq Hemani. 2013 08 14.
--              Rewritten the design to be more parametric where the
--              instruction formats are records.
--              Used a state machine (Moore) approach to switch between the different
--              modes of the Sequencer.
--              Added Refi1,Refi2,Refi3,DPU,Jump,Delay type of instructions.
--
-- Rev 2: Sadiq Hemani. 2013 08 20.
--              Changed state machine to be Mealy (from Moore) due to one cycle lost during
--              config state.
--
-- Rev 3: Sadiq Hemani. 2013 08 26.
--              Major changes include:
--              rewriting the decode logic to infer far less flops by
--              using combinatorial processes that enable registers when needed.
--              Added a variable pc, to be flexible enough to accomodate no-linear pc increment
--              modes when refi instructions include extensions.
--              Added swb instructions. (2013 10 10)
-- Rev 4: Nasim Farahini. 2014 02 26.   
-- Rev 5: Hassan Sohofi. 2015 02 22.
--              Adding branch instruction.    
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

--instr_ld is deasserted at the same time as the last instruction is being sent
--from the tb

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;
use work.top_consts_types_package.all;
use work.seq_functions_package.all;
use work.util_package.all;
use work.noc_types_n_constants.all;

entity sequencer is
	generic(
		-- ADDRESS : natural RANGE 0 TO 4*COLUMNS;
		M : natural);

	port(
		clk                : in  std_logic;
		rst_n              : in  std_logic;
		instr_ld           : in  std_logic;
		instr_inp          : in  std_logic_vector(INSTR_WIDTH - 1 downto 0);
		--seq_address     : IN std_logic_vector(SEQ_ADDRS_WIDTH-1 DOWNTO 0);  -- will mimic the address supplied on the address bus for the specific sequence
		seq_address_rb     : in  std_logic; -- range 0 to 4*ROWS;
		seq_address_cb     : in  std_logic; -- range 0 to 4*COLUMNS;
		seq_cond_status    : in  std_logic_vector(SEQ_COND_STATUS_WIDTH - 1 downto 0);

		--<DPU ports>--
		dpu_cfg            : out std_logic_vector(DPU_MODE_SEL_VECTOR_SIZE - 1 downto 0);
		dpu_ctrl_out_2     : out std_logic_vector(DPU_OUTP_A_VECTOR_SIZE - 1 downto 0);
		dpu_ctrl_out_3     : out std_logic_vector(DPU_OUTP_B_VECTOR_SIZE - 1 downto 0);
		dpu_acc_clear_rst  : out std_logic;
		dpu_acc_clear      : out std_logic_vector(DPU_ACC_CLEAR_WIDTH - 1 downto 0);
		dpu_sat_ctrl       : out std_logic_vector(DPU_SAT_CTRL_WIDTH - 1 downto 0);
        dpu_process_inout  : out std_logic_vector (DPU_PROCESS_INOUT_WIDTH-1 downto 0);

		--<AGU ports>--
		reg_port_type      : out std_logic_vector(NR_OF_REG_FILE_PORTS_VECTOR_SIZE - 1 downto 0);
		reg_start_addrs    : out std_logic_vector(STARTING_ADDRS_VECTOR_SIZE - 1 downto 0);
		reg_no_of_addrs    : out std_logic_vector(NR_OF_ADDRS_VECTOR_SIZE - 1 downto 0);
		reg_initial_delay  : out std_logic_vector(INIT_DELAY_VECTOR_SIZE - 1 downto 0);

		reg_step_val       : out std_logic_vector(STEP_VALUE_VECTOR_SIZE - 1 downto 0);
		reg_step_val_sign  : out std_logic_vector(STEP_VALUE_SIGN_VECTOR_SIZE - 1 downto 0);
		reg_middle_delay   : out std_logic_vector(REG_FILE_MIDDLE_DELAY_PORT_SIZE - 1 downto 0);
		reg_no_of_rpts     : out std_logic_vector(NUM_OF_REPT_PORT_SIZE - 1 downto 0);
		reg_rpt_step_value : out std_logic_vector(REP_STEP_VALUE_PORT_SIZE - 1 downto 0);
--		reg_dimarch_mode   : out std_logic_vector(NR_OF_REG_FILE_PORTS_VECTOR_SIZE-1 downto 0);
		reg_dimarch_mode   : out std_logic;
		reg_use_compr	   : out std_logic;
		instr_start        : out std_logic;

		reg_rpt_delay      : out std_logic_vector(REPT_DELAY_VECTOR_SIZE - 1 downto 0);
		reg_mode           : out std_logic_vector(MODE_SEL_VECTOR_SIZE - 1 downto 0);
		reg_outp_cntrl     : out std_logic_vector(OUTPUT_CONTROL_VECTOR_SIZE - 1 downto 0);
		reg_fft_stage      : out std_logic_vector(FFT_STAGE_SEL_VECTOR_SIZE - 1 downto 0);
		reg_end_fft_stage  : out std_logic_vector(FFT_STAGE_SEL_VECTOR_SIZE - 1 downto 0);

		NOC_BUS_OUT        : out NOC_BUS_TYPE;

		--<SWB port(s)>--
		--hc_h_bus_in : IN hc_in_bus_ty (0 TO 2*M + 1);

		-- DISCONNECTED, AWAITING LOCAL IMPLEMENTATION --
		--hc_out_left  : OUT std_logic_vector(HC_OUT_BITS-1 DOWNTO 0);    --   := (OTHERS => 'Z');  --hierarchical
		--control port
		--hc_out_right : OUT std_logic_vector(HC_OUT_BITS-1 DOWNTO 0);     --  := (OTHERS => 'Z');
		s_bus_out          : out std_logic_vector(SWB_INSTR_PORT_SIZE - 1 downto 0));

end;

architecture behv of sequencer is
	signal seq_address_match : std_logic; -- is asserted when the address from the bus matches the address of the sequencer.
	signal new_instr_ld      : std_logic; -- flags that a new instruction is ready
	signal instr_ld_counter  : unsigned(INSTR_REG_DEPTH - 1 downto 0); -- track the number of instrs in the instr. REGISTER
	signal delay_counter     : unsigned(DLY_CYCLES_VECTOR_SIZE - 1 downto 0);
	signal pc                : unsigned(PC_SIZE - 1 downto 0); -- program counter
	--SIGNAL pc                : signed(PC_SIZE DOWNTO 0);
	signal pc_increm         : std_logic_vector(PC_INCREM_WIDTH - 1 downto 0); --programmable increment value FOR
	signal pc_increm_set     : std_logic_vector(PC_INCREM_WIDTH - 1 downto 0);
	--pc = 1 by default
	signal instr_reg         : Instr_reg_ty; -- Instruction REGISTER

	signal valid_instr      : std_logic; -- flag to indicate that a valid instr has been detected in the "instr_code" field
	signal config_count_en  : std_logic; -- flag to indicate whether to load config instructions and increment instr_ld_counter
	--SIGNAL decode_mode     : std_logic;  -- flag to enable pc and instr. decode functionality
--	signal halt_instr       : std_logic; -- flag to indicate that the last
	-- instruction has been decoded
	signal instr            : std_logic_vector(INSTR_WIDTH - 1 downto 0); -- instruction to be decoded
--	signal refi1_instr      : Refi1_instr_ty;
--	signal refi2_instr      : Refi2_instr_ty;
--	signal refi3_instr      : Refi3_instr_ty;
	signal dpu_instr        : Dpu_instr_ty;
	signal branch_instr     : Branch_instr_ty;
	signal jump_instr       : Jump_instr_ty;
	signal delay_instr      : Delay_instr_ty;
	signal swb_instr        : std_logic_vector(INSTR_WIDTH - 1 downto 0);
	signal for_header_instr : for_header_instr_ty;
	signal for_tail_instr   : for_tail_instr_ty;
	signal raccu_instr      : raccu_instr_ty;

	signal reg_start_addrs_tmp   : std_logic_vector(STARTING_ADDRS_VECTOR_SIZE - 1 downto 0);
	signal reg_no_of_addrs_tmp   : std_logic_vector(NR_OF_ADDRS_VECTOR_SIZE - 1 downto 0);
	signal reg_initial_delay_tmp : std_logic_vector(INIT_DELAY_VECTOR_SIZE - 1 downto 0);
	signal reg_rpt_delay_tmp     : std_logic_vector(REPT_DELAY_VECTOR_SIZE - 1 downto 0);
	signal reg_mode_tmp          : std_logic_vector(MODE_SEL_VECTOR_SIZE - 1 downto 0);
	signal reg_outp_cntrl_tmp    : std_logic_vector(OUTPUT_CONTROL_VECTOR_SIZE - 1 downto 0);
	signal reg_fft_stage_tmp     : std_logic_vector(FFT_STAGE_SEL_VECTOR_SIZE - 1 downto 0);
	signal reg_end_fft_stage_tmp : std_logic_vector(FFT_STAGE_SEL_VECTOR_SIZE - 1 downto 0);

	signal reg_step_val_tmp       : std_logic_vector(STEP_VALUE_VECTOR_SIZE - 1 downto 0);
	signal reg_step_val_sign_tmp  : std_logic_vector(STEP_VALUE_SIGN_VECTOR_SIZE - 1 downto 0);
	signal reg_middle_delay_tmp   : std_logic_vector(REG_FILE_MIDDLE_DELAY_PORT_SIZE - 1 downto 0);
	signal reg_no_of_rpts_tmp     : std_logic_vector(NUM_OF_REPT_PORT_SIZE - 1 downto 0);
	signal reg_rpt_step_value_tmp : std_logic_vector(REP_STEP_VALUE_PORT_SIZE - 1 downto 0);
	signal reg_dimarch_mode_tmp   : std_logic;
	signal reg_use_compr_tmp	  : std_logic;
--	signal reg_dimarch_mode_tmp : std_logic_vector(NR_OF_REG_FILE_PORTS_VECTOR_SIZE-1 downto 0);

	--signal  del_cycles_tmp: std_logic_vector(DLY_CYCLES_VECTOR_SIZE DOWNTO 0);

	signal instr_refi2                                           : std_logic_vector(INSTR_WIDTH - 1 downto 0); --packed            
	signal instr_refi3                                           : std_logic_vector(INSTR_WIDTH - 1 downto 0); --packed
	signal dpu_acc_clear_tmp                                     : std_logic_vector(DPU_ACC_CLEAR_WIDTH - 1 downto 0);
	signal reg_outport_en                                        : std_logic;
	signal dpu_outport_en                                        : std_logic;
	signal swb_outport_en                                        : std_logic;
	signal pc_count_en                                           : std_logic;
	signal delay_count_en                                        : std_logic;
	signal delay_count_eq                                        : std_logic;
	signal non_lin_pc                                            : std_logic;
	signal jump_mode                                             : std_logic; -- indicates when a jump instruction is active
	signal jump_addrs                                            : std_logic_vector(PC_SIZE - 1 downto 0);
	signal subseq_refi_instrs                                    : std_logic;
	signal ext_flag_middle_delay                                 : std_logic;
	signal ext_flag_no_of_rpt                                    : std_logic;
	signal ext_flag_rpt_step_value, no_more_instr                : std_logic;
	signal instr_ld_counter_rst                                  : std_logic;
	signal pres_state, next_state                                : State_ty;
	--<RACCU>
	signal raccu_result_addrs_tmp                                : std_logic_vector(RACCU_RESULT_ADDR_VECTOR_SIZE - 1 downto 0);
	signal raccu_mode_tmp                                        : std_logic_vector(RACCU_MODE_SEL_VECTOR_SIZE - 1 downto 0);
	signal raccu_op1_tmp                                         : std_logic_vector(RACCU_OPERAND1_VECTOR_SIZE - 1 downto 0);
	signal raccu_op2_tmp                                         : std_logic_vector(RACCU_OPERAND2_VECTOR_SIZE - 1 downto 0);
	signal raccu_reg_out                                         : raccu_reg_out_ty;
	signal raccu_loop_reg                                        : raccu_loop_array_ty;
	signal raccu_op1_sd, raccu_op2_sd, loop_jump_mode, loop_mode : std_logic;
	--  SIGNAL instr_reg_en : std_logic;
	-- segmented bus output 
	alias O_BUS_ENABLE                                           : STD_LOGIC is NOC_BUS_OUT.bus_enable;
	alias O_instr_code                                           : std_logic_vector(INS_WIDTH - 1 downto 0) is NOC_BUS_OUT.instr_code;

	-- PATH SETUP FLAGS
	alias O_inter   : std_logic is NOC_BUS_OUT.INSTRUCTION(INTERMEDIATE_NODE_FLAG_l); -- IF 1 THEN INTERMEDIATE node instruction 
	alias O_inter_f : std_logic is NOC_BUS_OUT.INSTRUCTION(INTERMEDIATE_SEGMENT_FLAG_l); -- IF 1 THEN INTERMEDIATE segment source to intermediate 
	alias O_rw      : std_logic is NOC_BUS_OUT.INSTRUCTION(READ_WRITE_l); -- IF 1 write else read


	-- ADDRESSES 
	alias O_SR : std_logic_vector(ROW_WIDTH - 1 downto 0) is NOC_BUS_OUT.INSTRUCTION(SR_e downto SR_s);
	alias O_SC : std_logic_vector(COL_WIDTH - 1 downto 0) is NOC_BUS_OUT.INSTRUCTION(SC_e downto SC_s);
	alias O_DR : std_logic_vector(ROW_WIDTH - 1 downto 0) is NOC_BUS_OUT.INSTRUCTION(DR_e downto DR_s);
	alias O_DC : std_logic_vector(COL_WIDTH - 1 downto 0) is NOC_BUS_OUT.INSTRUCTION(DC_e downto DC_s);
	alias O_IR : std_logic_vector(ROW_WIDTH - 1 downto 0) is NOC_BUS_OUT.INSTRUCTION(IR_e downto IR_s);
	alias O_IC : std_logic_vector(COL_WIDTH - 1 downto 0) is NOC_BUS_OUT.INSTRUCTION(IC_e downto IC_s);

--	alias O_PR : STD_LOGIC is NOC_BUS_OUT.INSTRUCTION(PR_l); -- PRIORITY
	alias O_ON : STD_LOGIC is NOC_BUS_OUT.INSTRUCTION(ON_l); -- ORIGIN NODE 1=DESTINATION
	alias O_RF : std_logic_vector(1 downto 0) is NOC_BUS_OUT.INSTRUCTION(RF_e downto RF_s);

	-- manas instruction for route 
	alias i_src_row_addrs : std_logic_vector(ROW_WIDTH - 1 downto 0) is instr(INSTR_WIDTH - source_row_address_s downto INSTR_WIDTH - source_row_address_e);
	alias i_src_col_addrs : std_logic_vector(COL_WIDTH - 1 downto 0) is instr(INSTR_WIDTH - source_col_address_s downto INSTR_WIDTH - source_col_address_e);
	alias i_des_row_addrs : std_logic_vector(ROW_WIDTH - 1 downto 0) is instr(INSTR_WIDTH - dest_row_address_s downto INSTR_WIDTH - dest_row_address_e);
	alias i_des_col_addrs : std_logic_vector(COL_WIDTH - 1 downto 0) is instr(INSTR_WIDTH - dest_col_address_s downto INSTR_WIDTH - dest_col_address_e);
	alias i_inter         : STD_LOGIC is instr(INSTR_WIDTH - intermediate_l);
	alias i_int_row_addrs : std_logic_vector(ROW_WIDTH - 1 downto 0) is instr(INSTR_WIDTH - intr_row_address_s downto INSTR_WIDTH - intr_row_address_e);
	alias i_int_col_addrs : std_logic_vector(COL_WIDTH - 1 downto 0) is instr(INSTR_WIDTH - intr_col_address_s downto INSTR_WIDTH - intr_col_address_e);
--	alias i_priority      : STD_LOGIC is instr(INSTR_WIDTH - priority_l);
	alias i_Origin        : STD_LOGIC is instr(INSTR_WIDTH - Origin_l);
	--        alias i_RFNODE: STD_LOGIC 								IS instr(instr_width-        RFNODE_l 											   ); 
	alias i_RFNODE        : std_logic_vector(1 downto 0) is instr(INSTR_WIDTH - RFNODE_s downto INSTR_WIDTH - RFNODE_E);

--	signal branch_compare : std_logic_vector(1 downto 0);
--	signal verif          : std_logic;
	signal instr_1        : std_logic_vector(INSTR_WIDTH - 1 downto 0);
	signal instr_2        : std_logic_vector(INSTR_WIDTH - 1 downto 0);
	alias instr_code_alias : std_logic_vector(3 downto 0) is  instr(INSTR_CODE_RANGE_BASE downto INSTR_CODE_RANGE_END); -- this should be parameterized

begin                                   -- architecture behv
	--hc_out_left <= (OTHERS => '0');
	--hc_out_right <= (OTHERS => '0');
	--seq_address_match <= '1' WHEN (seq_address = SEQ_ADDRS) ELSE '0';
	seq_address_match <= seq_address_rb and seq_address_cb;
	--seq_address_match <= instr_inp(INSTR_WIDTH-1) AND seq_address_rb AND seq_address_cb;
	new_instr_ld      <= seq_address_match and instr_ld;

	-------------------------------------------------------------------------------------------------------------------
	--  RACCU Instanciation
	-------------------------------------------------------------------------------------------------------------------
	raccu_inst : entity work.RACCU
		port map(rst_n             => rst_n,
			     clk               => clk,
			     raccu_in1_sd      => raccu_op1_sd,
			     raccu_in1         => raccu_op1_tmp,
			     raccu_in2_sd      => raccu_op2_sd,
			     raccu_in2         => raccu_op2_tmp,
			     raccu_cfg_mode    => raccu_mode_tmp,
			     raccu_res_address => raccu_result_addrs_tmp,
			     raccu_regout      => raccu_reg_out,
			     raccu_loop_reg    => raccu_loop_reg);

	--<LOADS NEW INSTRUCTIONS TO THE MEMORY>--
	instr_reg_load_proc : process(clk, rst_n)
	begin                               -- PROCESS p0
		if rst_n = '0' then             -- asynchronous reset (active low)
			instr_reg <= (others => (others => '0'));
		elsif clk'event and clk = '1' then -- rising clock edge
			--IF instr_ld_counter = 0 THEN
			if (config_count_en = '1') and (new_instr_ld = '1') then --changed if condition from
				--"config_count_en = '1'" to
				--"new_instr_ld = '1'"
				instr_reg(to_integer(instr_ld_counter)) <= instr_inp;
			end if;
		--ELSE
		--IF config_count_en = '1' THEN
		--instr_reg(to_integer(instr_ld_counter)) <= instr_inp(INSTR_WIDTH-1 downto 0);
		--END IF;
		--END IF;
		end if;
	end process instr_reg_load_proc;

	--<INCREMENTS INSTRUCTION LOAD COUNTER>--
	pc_increm_proc : process(clk, rst_n)
	begin                               -- PROCESS p1
		if rst_n = '0' then             -- asynchronous reset (active low)
			instr_ld_counter <= (others => '0');
		elsif clk'event and clk = '1' then -- rising clock edge
			if (config_count_en = '1') then
				--IF (instr_ld_counter > INSTR_DEPTH-1) THEN
				--instr_ld_counter <= (OTHERS => '0');  --CHECK IF THIS IS NEEDED,
				--JINGYING FOUND A POTENTIAL
				--BUG HERE
				--ELSE
				if (instr_ld_counter < INSTR_DEPTH - 1 and new_instr_ld = '1') then
					instr_ld_counter <= instr_ld_counter + 1;
				end if;
			end if;
			if (instr_ld_counter_rst = '1') then
				instr_ld_counter <= (others => '0');
			end if;
		end if;
	end process pc_increm_proc;

	instr     <= instr_reg(to_integer(pc)); -- WHEN pc_count_en = '1' ELSE (OTHERS => '0');
	instr_1   <= instr_reg(to_integer(pc + 1));
	instr_2   <= instr_reg(to_integer(pc + 2));
	pc_increm <= pc_increm_set when non_lin_pc = '1' else "01";

	--no_more_instr <= '1' WHEN instr_ld_counter-1 = pc ELSE '0';

	--<DETECTS WHEN NO MORE INSTRUCTIONS ARE AVAILABLE>--
	no_more_instr_proc : process(instr_ld_counter, pc, pc_count_en, delay_count_eq, loop_mode) is
	begin                               -- PROCESS no_more_instr_proc

		no_more_instr <= '0';

		if (pc_count_en = '1') then
			if (instr_ld_counter = 1) then -- single instruction case
				if (pc = 1) or (delay_count_eq = '1') then --
					no_more_instr <= '1';
				--ELSE
				--  no_more_instr <= '0';
				end if;
			else
				if ((pc = instr_ld_counter - 1) or (pc = instr_ld_counter)) and (pc >= 1) and loop_mode = '0' then
					no_more_instr <= '1';

				--ELSE
				--  no_more_instr <= '0';
				end if;
			end if;
		end if;
	end process no_more_instr_proc;

	--<PC MANAGEMENT AND REG>--
	pc_reg : process(clk, rst_n) is
	begin                               -- PROCESS pc_reg
		if rst_n = '0' then             -- asynchronous reset (active low)
			pc <= (others => '0');

		elsif clk'event and clk = '1' then -- rising clock edge
			if delay_count_en = '0' then
				if pc_count_en = '1' then
					if jump_mode = '1' or loop_jump_mode = '1' then
						pc <= unsigned(jump_addrs);
					else
						-- SHOULD CHECK IF PC EXCEEDS THE INSTR_DEPTH BEFORE INCREMENTING
						pc <= pc + unsigned(pc_increm);
					end if;             -- jump_mode
				else
					--if do_not_goto_pczero = '0' then
					pc <= (others => '0');
				--end if;

				end if;                 -- pc_count_en
			end if;                     -- delay_on
		end if;
	end process pc_reg;

	--<DELAY COUNTER>--
	del_cnt : process(clk, rst_n) is
	begin                               -- PROCESS del_cnt
		if rst_n = '0' then             -- asynchronous reset (active low)
			delay_counter <= (others => '0');
		elsif clk'event and clk = '1' then -- rising clock edge
			if delay_count_eq = '0' and delay_count_en = '1' then
				delay_counter <= delay_counter + 1;
			else
				delay_counter <= (others => '0');
			end if;
		end if;
	end process del_cnt;

	dpu_out : process(clk, rst_n) is
	begin                               -- PROCESS reg
		if rst_n = '0' then             -- asynchronous reset (active low)
			dpu_cfg           <= (others => '0');
			dpu_ctrl_out_2    <= (others => '0');
			dpu_ctrl_out_3    <= (others => '0');
			dpu_acc_clear     <= (others => '0');
			dpu_sat_ctrl      <= (others => '0');
			dpu_process_inout <= (others => '0');
			dpu_acc_clear_rst <= '0';
		elsif clk'event and clk = '1' then -- rising clock edge

			if dpu_outport_en = '1' then
				-- TODO:
				-- remove the safty check for mode less than 15
				-- extra mode need more space
				-- Already removed
				--if (unpack_dpu_record(instr).dpu_mode > "001111") then -- manually added "fail safe" to not get any unwanted cfg_mode, can be removed if neccessary.
				--	dpu_cfg <= (others => '0');
				--else
					dpu_cfg           <= unpack_dpu_record(instr).dpu_mode;
					dpu_ctrl_out_2    <= unpack_dpu_record(instr).dpu_out_a;
					dpu_ctrl_out_3    <= unpack_dpu_record(instr).dpu_out_b;
					dpu_acc_clear     <= dpu_acc_clear_tmp; --unpack_dpu_record(instr).dpu_acc_clear;
					dpu_sat_ctrl      <= unpack_dpu_record(instr).dpu_saturation;
					dpu_acc_clear_rst <= unpack_dpu_record(instr).dpu_acc_clear_rst;
					dpu_process_inout <= unpack_dpu_record(instr).dpu_process_inout;
				--end if;
			else
				dpu_acc_clear_rst <= '0';
			end if;

		end if;
	end process dpu_out;

	swb_out : process(clk, rst_n) is
	begin                               -- PROCESS swb_out
		if rst_n = '0' then             -- asynchronous reset (active low)
			s_bus_out <= (others => '0');
		elsif clk'event and clk = '1' then -- rising clock edge

			if swb_outport_en = '1' then
				s_bus_out <= swb_instr(INSTR_WIDTH - 5 downto 12); --holds the entire instruction as it IS
			--to be forwarded to the s_bus_outp port
			end if;

		end if;
	end process swb_out;

	reg_out : process(clk, rst_n) is
	begin                               -- PROCESS reg_rg
		if rst_n = '0' then             -- asynchronous reset (active low)
			reg_port_type      <= (others => '0');
			reg_start_addrs    <= (others => '0');
			reg_no_of_addrs    <= (others => '0');
			reg_initial_delay  <= (others => '0');
			reg_step_val       <= (others => '0');
			reg_step_val_sign  <= (others => '0');
			reg_middle_delay   <= (others => '0');
			reg_no_of_rpts     <= (others => '0');
			reg_rpt_step_value <= (others => '0');
			reg_rpt_delay      <= (others => '0');
			reg_mode           <= (others => '0');
			reg_outp_cntrl     <= (others => '0');
			reg_fft_stage      <= (others => '0');
			reg_end_fft_stage  <= (others => '0');
			instr_start        <= '0';
			reg_dimarch_mode   <= '0';
--			reg_dimarch_mode   <= (others => '0');
			reg_use_compr	   <= '0';

		elsif clk'event and clk = '1' then -- rising clock edge

			if reg_outport_en = '1' then
				reg_port_type      <= unpack_refi1_record(instr).reg_file_port;
				reg_start_addrs    <= reg_start_addrs_tmp; --unpack_refi1_record(instr).start_addrs;
				reg_no_of_addrs    <= reg_no_of_addrs_tmp; --unpack_refi1_record(instr).no_of_addrs;
				reg_initial_delay  <= reg_initial_delay_tmp; --unpack_refi1_record(instr).initial_delay;
				instr_start        <= '1';
				reg_rpt_delay      <= reg_rpt_delay_tmp;
				reg_mode           <= reg_mode_tmp;
				reg_outp_cntrl     <= reg_outp_cntrl_tmp;
				reg_fft_stage      <= reg_fft_stage_tmp;
				reg_end_fft_stage  <= reg_end_fft_stage_tmp;
				reg_step_val       <= reg_step_val_tmp;
				reg_step_val_sign  <= reg_step_val_sign_tmp;
				reg_middle_delay   <= reg_middle_delay_tmp;
				reg_no_of_rpts     <= reg_no_of_rpts_tmp;
				reg_rpt_step_value <= reg_rpt_step_value_tmp;
				reg_dimarch_mode   <= reg_dimarch_mode_tmp;
				reg_use_compr	   <= reg_use_compr_tmp;	

			else
				instr_start <= '0';
			end if;
		end if;
	end process reg_out;

	--<STATE MACHINES>--

	c0 : process(pres_state, raccu_reg_out, raccu_loop_reg, new_instr_ld, instr_ld, instr, delay_counter, instr_ld_counter, pc, instr_reg, instr_refi2, instr_refi3, no_more_instr, seq_cond_status, instr_1, instr_2) --MEALY

		variable del_cycles_tmp : unsigned(DLY_CYCLES_VECTOR_SIZE downto 0) := (others => '0');
--		variable d_sr_dr        : std_logic_vector(ROW_WIDTH - 1 downto 0);
--		variable d_sr_ir        : std_logic_vector(ROW_WIDTH - 1 downto 0);
--		variable d_ir_dr        : std_logic_vector(ROW_WIDTH - 1 downto 0);
--		variable d_sc_dc        : std_logic_vector(COL_WIDTH - 1 downto 0);
--		variable d_sc_ic        : std_logic_vector(COL_WIDTH - 1 downto 0);
--		variable d_ic_dc        : std_logic_vector(COL_WIDTH - 1 downto 0);
		variable sram_instruction : sram_instr_type;
		

	begin                               -- PROCESS c0
		NOC_BUS_OUT.INSTRUCTION <= (others => '0');
		NOC_BUS_OUT <= IDLE_BUS;
--		halt_instr              <= '0';
		valid_instr             <= '1';
		--config_count_en          <= '0';
		subseq_refi_instrs      <= '0';
		ext_flag_middle_delay   <= '0';
		ext_flag_no_of_rpt      <= '0';
		ext_flag_rpt_step_value <= '0';
		--instr_reg_en           <= '0';

		delay_count_eq <= '0';
		delay_count_en <= '0';
		reg_outport_en <= '0';
		dpu_outport_en <= '0';
		swb_outport_en <= '0';

		--instr_start   <= '0';
		non_lin_pc    <= '0';
		pc_increm_set <= "01";

		instr_refi2 <= (others => '0');
		instr_refi3 <= (others => '0');

--		refi1_instr            <= REFI1_INIT;
--		refi2_instr            <= ((others => '0'), '0', (others => '0'), (others => '0'), '0', (others => '0'), '0', (others => '0'), (others => '0'));
		dpu_instr              <= ((others => '0'), (others => '0'), (others => '0'), (others => '0'), (others => '0'), '0', '0', (others => '0'), (others => '0'));
		branch_instr           <= ((others => '0'), (others => '0'), (others => '0'), (others => '0'));
		jump_instr             <= ((others => '0'), (others => '0'), (others => '0'));
		delay_instr            <= ((others => '0'), '0', (others => '0'), (others => '0'));
		for_header_instr       <= ((others => '0'), (others => '0'), (others => '0'), '0', (others => '0'), (others => '0'));
		for_tail_instr         <= ((others => '0'), (others => '0'), (others => '0'), (others => '0'), (others => '0'));
		raccu_instr            <= ((others => '0'), (others => '0'), '0', (others => '0'), '0', (others => '0'), (others => '0'));
		swb_instr              <= (others => '0');
		reg_rpt_delay_tmp      <= (others => '0');
		reg_mode_tmp           <= "0";
		reg_outp_cntrl_tmp     <= "11";
		reg_fft_stage_tmp      <= (others => '0');
		reg_end_fft_stage_tmp  <= (others => '0');
		reg_step_val_tmp       <= (others => '0');
		reg_step_val_sign_tmp  <= (others => '0');
		reg_middle_delay_tmp   <= (others => '0');
		reg_no_of_rpts_tmp     <= (others => '0');
		reg_rpt_step_value_tmp <= (others => '0');
--		reg_dimarch_mode_tmp   <= (others => '0');
		reg_dimarch_mode_tmp   <= '0';
		reg_use_compr_tmp	   <= '0';
		config_count_en        <= '0';
		loop_jump_mode         <= '0';
		pc_count_en            <= '0';
		instr_ld_counter_rst   <= '0';
		raccu_mode_tmp         <= (others => '0');
		raccu_op1_tmp          <= (others => '0');
		raccu_op2_tmp          <= (others => '0');
		raccu_op1_sd           <= '0';
		raccu_op2_sd           <= '0';
		raccu_result_addrs_tmp <= (others => '0');
		--del_cycles_tmp <= (OTHERS  => '0');
		dpu_acc_clear_tmp      <= (others => '0');
		reg_start_addrs_tmp    <= (others => '0');
		reg_no_of_addrs_tmp    <= (others => '0');
		reg_initial_delay_tmp  <= (others => '0');
		loop_mode              <= '0';

		jump_mode  <= '0';
		jump_addrs <= (others => '0');

		-- do_not_goto_pczero <= '0';
		next_state <= pres_state;

		case pres_state is
			when IDLE_ST =>
				pc_count_en <= '0';
				--do_not_goto_pczero <= '1';

				if (new_instr_ld = '1') then
					next_state      <= SEQ_LOADING_ST;
					config_count_en <= '1';
				--instr_reg_en    <= '0';


				end if;

			when SEQ_LOADING_ST =>
				config_count_en <= '1';

				if (instr_ld = '0') then
					next_state      <= INSTR_DECODE_ST; -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
					config_count_en <= '0'; --let config_count_en be asserted for
					--one cycle extra in order to be able
					--to decode the first instruction as
					--well as to write the last instruction
					--in the instruction register in the
					--same cycle
					pc_count_en     <= '0';

				end if;

			when INSTR_DECODE_ST => 
				config_count_en <= '0';

				if no_more_instr = '1' then
					next_state           <= IDLE_ST;
					pc_count_en          <= '0';
					instr_ld_counter_rst <= '1';

					case instr(INSTR_CODE_RANGE_BASE downto INSTR_CODE_RANGE_END) is
						when REFI1 | REFI2 | REFI3 =>
							reg_outport_en <= '1';
						when DPU =>
							dpu_outport_en <= '1';
						when SWB =>
							swb_outport_en <= '1';
						when DELAY =>
							delay_instr <= unpack_delay_record(instr);

							delay_count_en <= '1';
							pc_count_en    <= '0'; -- stop pc for the number of cycles
						-- given in the delay instruction.
						when others => null;
					end case;

				end if;

				--ELSE--commented to solve a bug where the sequencer didn't decode
				--the last instruction

				pc_count_en <= '1';

				case instr_code_alias is
					when REFI1 | REFI2 | REFI3 =>
--						if instr(INSTR_CODE_RANGE_BASE downto INSTR_CODE_RANGE_END) = REFI1 then
--							refi1_instr <= unpack_refi1_record(instr);
--						elsif instr(INSTR_CODE_RANGE_BASE downto INSTR_CODE_RANGE_END) = REFI2 then
--							refi2_instr <= unpack_refi2_record(instr);
--						elsif instr(INSTR_CODE_RANGE_BASE downto INSTR_CODE_RANGE_END) = REFI3 then
--							refi3_instr <= unpack_refi3_record(instr);
--						end if;

						if unpack_refi1_record(instr).start_addrs_sd = '0' then
							reg_start_addrs_tmp <= unpack_refi1_record(instr).start_addrs;
						else
							reg_start_addrs_tmp <= raccu_reg_out(CONV_INTEGER(unpack_refi1_record(instr).start_addrs))(STARTING_ADDRS_VECTOR_SIZE - 1 downto 0);
						end if;

						if unpack_refi1_record(instr).no_of_addrs_sd = '0' then
							reg_no_of_addrs_tmp <= unpack_refi1_record(instr).no_of_addrs;
						else
							reg_no_of_addrs_tmp <= raccu_reg_out(CONV_INTEGER(unpack_refi1_record(instr).no_of_addrs))(NR_OF_ADDRS_VECTOR_SIZE - 1 downto 0);
						end if;

						if unpack_refi1_record(instr).initial_delay_sd = '0' then
							reg_initial_delay_tmp <= unpack_refi1_record(instr).initial_delay;
						else
							reg_initial_delay_tmp <= raccu_reg_out(CONV_INTEGER(unpack_refi1_record(instr).initial_delay(RACCU_REG_ADDRS_WIDTH - 1 downto 0)))(INIT_DELAY_VECTOR_SIZE - 1 downto 0);
						end if;

						reg_outport_en <= '1';

						if unpack_refi1_record(instr).subseq_instrs = "00" or unpack_refi1_record(instr).subseq_instrs = "11" then
							reg_rpt_delay_tmp      <= (others => '0');
							reg_mode_tmp           <= "0";
							reg_outp_cntrl_tmp     <= "11";
							reg_fft_stage_tmp      <= (others => '0');
							reg_end_fft_stage_tmp  <= (others => '0');
							reg_step_val_tmp       <= "000001";
							reg_step_val_sign_tmp  <= (others => '0');
							reg_middle_delay_tmp   <= (others => '0');
							reg_no_of_rpts_tmp     <= (others => '0');
							reg_rpt_step_value_tmp <= (others => '0');
							reg_rpt_step_value_tmp <= (others => '0');

						else
							instr_refi2 <= instr_reg(to_integer(pc + 1));

							if unpack_refi1_record(instr).subseq_instrs = "01" then

								--instr_refi2 <= instr_reg(to_integer(pc + 1));

								pc_increm_set          <= "10";
								non_lin_pc             <= '1';
								reg_rpt_delay_tmp      <= (others => '0');
								reg_mode_tmp           <= "0";
								reg_outp_cntrl_tmp     <= "11";
								reg_fft_stage_tmp      <= (others => '0');
								reg_end_fft_stage_tmp  <= (others => '0');
								reg_step_val_tmp       <= unpack_refi2_record(instr_refi2).step_val;
								reg_step_val_sign_tmp  <= unpack_refi2_record(instr_refi2).step_val_sign;
								reg_middle_delay_tmp   <= "00" & unpack_refi2_record(instr_refi2).refi_middle_delay;
								reg_no_of_rpts_tmp     <= "0" & unpack_refi2_record(instr_refi2).no_of_reps;
								reg_rpt_step_value_tmp <= "00" & unpack_refi2_record(instr_refi2).rpt_step_value;

							elsif unpack_refi1_record(instr).subseq_instrs = "10" then

								--instr_refi2 <= instr_reg(to_integer(pc + 1));
								instr_refi3 <= instr_reg(to_integer(pc + 2));-- shouldn't this be a variable??

								pc_increm_set          <= "11";
								non_lin_pc             <= '1';
								reg_mode_tmp           <= unpack_refi3_record(instr_refi3).mode;
								reg_step_val_tmp       <= unpack_refi2_record(instr_refi2).step_val;-- why is this refi2 value
								reg_outp_cntrl_tmp     <= unpack_refi3_record(instr_refi3).outp_cntrl;
								reg_fft_stage_tmp      <= unpack_refi3_record(instr_refi3).fft_stage;
								reg_end_fft_stage_tmp  <= unpack_refi3_record(instr_refi3).end_fft_stage;
								reg_middle_delay_tmp   <= unpack_refi3_record(instr_refi3).refi_middle_delay_ext & unpack_refi2_record(instr_refi2).refi_middle_delay;
								reg_no_of_rpts_tmp     <= unpack_refi3_record(instr_refi3).no_of_rpt_ext & unpack_refi2_record(instr_refi2).no_of_reps;
								reg_rpt_step_value_tmp <= unpack_refi3_record(instr_refi3).rpt_step_value_ext & unpack_refi2_record(instr_refi2).rpt_step_value;
								reg_dimarch_mode_tmp   <= unpack_refi3_record(instr_refi3).dimarch_mode;
								reg_use_compr_tmp 	   <= unpack_refi3_record(instr_refi3).use_compr;
--								if unpack_refi3_record(instr_refi3).dimarch_mode = '1' then
--									reg_dimarch_mode_tmp <= unpack_refi1_record(instr).reg_file_port;
--								else
--									reg_dimarch_mode_tmp <= (others => '0');
--								end if;

								if unpack_refi3_record(instr_refi3).rpt_delay_sd = '0' then
									reg_rpt_delay_tmp <= unpack_refi3_record(instr_refi3).rpt_delay;
								else
									reg_rpt_delay_tmp <= raccu_reg_out(CONV_INTEGER(unpack_refi3_record(instr_refi2).rpt_delay(RACCU_REG_ADDRS_WIDTH - 1 downto 0)))(REPT_DELAY_VECTOR_SIZE - 1 downto 0);
								end if;

							end if;

							if unpack_refi2_record(instr_refi2).step_val_sd = '1' then
								reg_step_val_tmp <= raccu_reg_out(CONV_INTEGER(unpack_refi2_record(instr_refi2).step_val(RACCU_REG_ADDRS_WIDTH - 1 downto 0)))(STEP_VALUE_VECTOR_SIZE - 1 downto 0);
							end if;

							if unpack_refi2_record(instr_refi2).refi_middle_delay_sd = '1' then
								reg_middle_delay_tmp <= raccu_reg_out(CONV_INTEGER(unpack_refi2_record(instr_refi2).refi_middle_delay(RACCU_REG_ADDRS_WIDTH - 1 downto 0)))(REG_FILE_MIDDLE_DELAY_PORT_SIZE - 1 downto 0);
							end if;

							if unpack_refi2_record(instr_refi2).no_of_reps_sd = '1' then
								reg_no_of_rpts_tmp <= raccu_reg_out(CONV_INTEGER(unpack_refi2_record(instr_refi2).no_of_reps(RACCU_REG_ADDRS_WIDTH - 1 downto 0)))(NUM_OF_REPT_PORT_SIZE - 1 downto 0);
							end if;

						--if unpack_refi2_record(instr_refi2).rpt_step_value_sd = '0' then 
						--	reg_rpt_step_value_tmp <= raccu_reg_out (CONV_INTEGER(unpack_refi2_record(instr_refi2).rpt_step_value));
						--end if;

						end if;

					when DPU =>
						dpu_instr         <= unpack_dpu_record(instr);
						--if unpack_dpu_record(instr).dpu_acc_clear_sd='0' then 
						dpu_acc_clear_tmp <= unpack_dpu_record(instr).dpu_acc_clear;
						--else 
						--    dpu_acc_clear_tmp <= raccu_reg_out (CONV_INTEGER(unpack_dpu_record(instr).dpu_acc_clear))(DPU_ACC_CLEAR_WIDTH-1 DOWNTO 0);
						--end if;

						dpu_outport_en <= '1';

					when SWB =>
						swb_instr <= instr;

						swb_outport_en <= '1';

					when BRANCH =>
						branch_instr <= unpack_branch_record(instr);

						if (unpack_branch_record(instr).brnch_mode and seq_cond_status) = "00" then -- false
							jump_addrs <= unpack_branch_record(instr).brnch_false_addr;
							jump_mode  <= '1';
						end if;

					when JUMP =>
						jump_instr <= unpack_jump_record(instr);

						jump_addrs <= unpack_jump_record(instr).true_addrs;
						jump_mode  <= '1';

					when DELAY =>
						delay_instr    <= unpack_delay_record(instr);
						delay_count_en <= '1';
						pc_count_en    <= '0'; -- stop pc for the number of cycles
						-- given in the delay instruction.

						if unpack_delay_record(instr).del_cycles_sd = '0' then --bug for delta delay
							del_cycles_tmp := unsigned(unpack_delay_record(instr).del_cycles);

							if delay_counter = unsigned(unpack_delay_record(instr).del_cycles) then
								delay_count_en <= '0';
								delay_count_eq <= '1';
								pc_count_en    <= '1'; -- resumes pc when delay_counter == the number of cycles given in the delay instruction
							end if;
						else
							del_cycles_tmp := resize(unsigned(raccu_reg_out(CONV_INTEGER(unpack_delay_record(instr).del_cycles))), del_cycles_tmp'length);

							if delay_counter = del_cycles_tmp then --bug for delta delay
								delay_count_en <= '0';
								delay_count_eq <= '1';
								pc_count_en    <= '1'; -- resumes pc when delay_counter == the number of cycles given in the delay instruction
							end if;
						end if;

					when HALT =>
						next_state      <= IDLE_ST;
						config_count_en <= '0';
						pc_count_en     <= '0';

					when FOR_HEADER =>
						for_header_instr <= unpack_for_header_record(instr);
						raccu_mode_tmp   <= "001"; --loop header mode
						raccu_op1_tmp    <= std_logic_vector(resize(unsigned(unpack_for_header_record(instr).index_start), raccu_op1_tmp'length));
						if unpack_for_header_record(instr).iter_no_sd = '0' then
							raccu_op2_tmp <= std_logic_vector(resize(unsigned(unpack_for_header_record(instr).iter_no), raccu_op2_tmp'length)); -- raccu should store these two vals in consequetive regs   
						else
							raccu_op2_tmp <= raccu_reg_out(CONV_INTEGER(unpack_for_header_record(instr).iter_no));
						end if;
						raccu_result_addrs_tmp <= unpack_for_header_record(instr).index_raccu_addr; -- which loop it is ? (loop register address)

					when FOR_TAIL =>
						for_tail_instr         <= unpack_for_tail_record(instr);
						raccu_mode_tmp         <= "010"; --loop tail mode
						raccu_op1_tmp          <= std_logic_vector(resize(unsigned(unpack_for_tail_record(instr).index_step), raccu_op1_tmp'length));
						raccu_op2_tmp          <= (others => '0');
						raccu_result_addrs_tmp <= unpack_for_tail_record(instr).index_raccu_addr; --address to determine which pc to go.
						if raccu_loop_reg(CONV_INTEGER(unpack_for_tail_record(instr).index_raccu_addr)).loop_end_flag = '0' then
							loop_jump_mode <= '1';
							jump_addrs     <= unpack_for_tail_record(instr).pc_togo;
							loop_mode      <= '1';
						end if;

					when RACCU =>
						raccu_instr            <= unpack_raccu_record(instr);
						raccu_result_addrs_tmp <= unpack_raccu_record(instr).raccu_result_addrs;
						raccu_mode_tmp         <= unpack_raccu_record(instr).raccu_mode;
						if unpack_raccu_record(instr).raccu_op1_sd = '0' then
							raccu_op1_tmp <= unpack_raccu_record(instr).raccu_op1;
						else
							raccu_op1_tmp <= raccu_reg_out(CONV_INTEGER(unpack_raccu_record(instr).raccu_op1));
						end if;
						if unpack_raccu_record(instr).raccu_op2_sd = '0' then
							raccu_op2_tmp <= unpack_raccu_record(instr).raccu_op2;
						else
							raccu_op2_tmp <= raccu_reg_out(CONV_INTEGER(unpack_raccu_record(instr).raccu_op2));
						end if;
					when ROUTE =>
						O_BUS_ENABLE            <= '1';
						O_instr_code            <= both_instruction;
						NOC_BUS_OUT.INSTRUCTION <= (others => '0');
						O_SR                    <= i_src_row_addrs;
						O_SC                    <= i_src_col_addrs;
						O_DR                    <= i_des_row_addrs;
						O_DC                    <= i_des_col_addrs;
						O_IR                    <= i_int_row_addrs;
						O_IC                    <= i_int_col_addrs;


						O_inter   <= i_inter;
						O_inter_f <= '0';
						O_rw      <= '0';
						O_ON      <= i_Origin;
						O_RF      <= i_RFNODE;

					when READ_SRAM | WRITE_SRAM =>
						------------------------------------------------- UNPACK  -------------------------------------------------
						-- this way off making bits the same size is not parameteized and needs to be updated
						sram_instruction :=unpack_sram_tb_instruction(instr(INSTR_WIDTH -1 downto 0) & instr_1(INSTR_WIDTH - 1 downto 0) & instr_2(INSTR_WIDTH - 1 downto 0));
						------------------------------------------------- STATIC DYNAMIC  -------------------------------------------------
							-- following codes loads the dynamic values from the RACCU register
							-- loop1 iteration & increment is parameterized and can be changed based on different block sizes of RF. 
							-- It can either be bigger or smaller then RACCU so we have to handle sign extention for both cases 
							-- since the value is unsigned this value will be zero for all unsigned values 
							-- increments are signed so they will require forllowing logic 

						------------------------------------------------- Initial -------------------------------------------------
						if sram_instruction.sram_Initial_address_sd = '1' then -- initial Address Static or Dynamic
							sram_instruction.Initial_Address := raccu_reg_out(CONV_INTEGER(sram_instruction.Initial_Address(RACCU_REG_ADDRS_WIDTH - 1 downto 0)));
						end if;
						if sram_instruction.sram_initial_delay_sd = '1' then  -- initial Delay Static or Dynamic
							sram_instruction.Initial_Delay :=raccu_reg_out(CONV_INTEGER(sram_instruction.Initial_Delay(RACCU_REG_ADDRS_WIDTH - 1 downto 0)))(INITIAL_DELAY_WIDTH - 1 downto 0);
						end if;
						
						
						
						------------------------------------------------- Loop 1 ------------------------------------------------- 
						if sram_instruction.sram_Loop1_iteration_sd = '1' then  -- Loop 1 iteration  Static or Dynamic
							if sr_loop1_iteration_width > RACCU_REG_ADDRS_WIDTH then
								sram_instruction.Loop1_iteration(RACCU_REG_BITWIDTH-1 downto 0) := raccu_reg_out(CONV_INTEGER(sram_instruction.Loop1_iteration(RACCU_REG_ADDRS_WIDTH-1 downto 0)));
								sram_instruction.Loop1_iteration(sr_loop1_iteration_width-1 downto RACCU_REG_ADDRS_WIDTH) := (others => '0'); -- unsgined sign extention
								-- ORIGINAL LINE:	
								--sram_instruction.Loop1_iteration(sr_loop1_iteration_width-1 downto RACCU_REG_BITWIDTH) := (others => '0'); -- unsgined sign extention
							else    --  When RACCU is bigger then iteration  just un comment following lines 
								assert false report "RACCU Width is bigger then loop1 interations uncomment the lines below this assert statement to make the design work" severity error;
								--	sram_instruction.Loop1_iteration(sr_loop1_iteration_width-1 downto 0) :="0"&raccu_reg_out(CONV_INTEGER(sram_instruction.Loop1_iteration(RACCU_REG_ADDRS_WIDTH-1 downto 0)));
								--  -- no signed extention is required here 
							end if;
							
						end if;
						
						if sram_instruction.sram_Loop1_increment_sd = '1' then -- Loop 1 increment  Static or Dynamic
							if sr_loop1_iteration_width > RACCU_REG_ADDRS_WIDTH then
								sram_instruction.Loop1_Increment(RACCU_REG_BITWIDTH-1 downto 0) := raccu_reg_out(CONV_INTEGER(sram_instruction.Loop1_Increment(RACCU_REG_ADDRS_WIDTH-1 downto 0)));
								if sram_instruction.Loop1_Increment(RACCU_REG_BITWIDTH-1) = '1' then -- sign extentions 
									sram_instruction.Loop1_Increment(sr_loop1_increment_width-1 downto RACCU_REG_ADDRS_WIDTH) := (others => '1');
								else 
									sram_instruction.Loop1_Increment(sr_loop1_increment_width-1 downto RACCU_REG_ADDRS_WIDTH) := (others => '0');
								end if;
							else
								assert false report "RACCU Width is bigger then loop1 increment uncomment the lines below this assert statement to make the design work" severity error;
--								sram_instruction.Loop1_Increment := raccu_reg_out(CONV_INTEGER(sram_instruction.Loop1_Increment(RACCU_REG_ADDRS_WIDTH-1 downto 0))) (sr_loop1_increment_width-1 downto 0);
							end if;
							
						end if;
						
						if sram_instruction.sram_Loop1_delay_sd = '1' then  -- Loop 1 Delay  Static or Dynamic
							sram_instruction.Loop1_Delay := raccu_reg_out(CONV_INTEGER(sram_instruction.Loop1_Delay(RACCU_REG_ADDRS_WIDTH-1 downto 0)))(sr_loop1_delay_width-1 downto 0);
						end if;
						
						------------------------------------------------- Loop 2 -------------------------------------------------
						--- assumtions   
						--RACCU_REG_ADDRS_WIDTH is always smaller then iteration and increment
						-- RACCU_REG_ADDRS_WIDTH is equal to delay size 

						if sram_instruction.sram_Loop2_iteration_sd = '1' then  -- Loop 1 iteration  Static or Dynamic
							sram_instruction.Loop2_iteration(RACCU_REG_BITWIDTH-1 downto 0) := raccu_reg_out(CONV_INTEGER(sram_instruction.Loop2_iteration(RACCU_REG_ADDRS_WIDTH-1 downto 0)));
							sram_instruction.Loop2_iteration(sr_loop2_iteration_width-1 downto RACCU_REG_ADDRS_WIDTH) := (others => '0'); -- unsgined sign extention
						end if;
						
						if sram_instruction.sram_Loop2_increment_sd = '1' then -- Loop 1 increment  Static or Dynamic
							sram_instruction.Loop2_Increment := '0' & raccu_reg_out(CONV_INTEGER(sram_instruction.Loop2_Increment(RACCU_REG_ADDRS_WIDTH-1 downto 0)));
							--ORIGINAL LINE: 
							--sram_instruction.Loop2_Increment := raccu_reg_out(CONV_INTEGER(sram_instruction.Loop2_Increment(RACCU_REG_ADDRS_WIDTH-1 downto 0)));
							if sram_instruction.Loop2_Increment(RACCU_REG_ADDRS_WIDTH-1) = '1' then -- sign extentions 
								sram_instruction.Loop2_Increment(sr_loop2_increment_width-1 downto RACCU_REG_ADDRS_WIDTH) := (others => '1');
							else 
								sram_instruction.Loop2_Increment(sr_loop2_increment_width-1 downto RACCU_REG_ADDRS_WIDTH) := (others => '0');
							end if;
						end if;
						if sram_instruction.sram_Loop2_delay_sd = '1' then  -- Loop 2 Delay  Static or Dynamic
							sram_instruction.Loop2_Delay := raccu_reg_out(CONV_INTEGER(sram_instruction.Loop2_Delay(RACCU_REG_ADDRS_WIDTH-1 downto 0)))(sr_loop2_delay_width-1 downto 0);
						end if;
						
						 
						------------------------------------------------- READ/WRITE -------------------------------------------------
						if instr_code_alias = WRITE_SRAM then 
							sram_instruction.rw  := '1';
						else  
							sram_instruction.rw  := '0';
						end if;
						------------------------------------------------- SEND on NoC Bus -------------------------------------------------
						NOC_BUS_OUT <= pack_sram_noc_instruction(sram_instruction); -- sram instruction is placed on noc bus 
						------------------------------------------------- Increment PC -------------------------------------------------
						-- increment is set to 3 as this is a three part instruction
						pc_increm_set          <= "11"; 
						non_lin_pc  <= '1';
--					when WRITE_SRAM =>
----						NOC_BUS_OUT.INSTRUCTION <= instr(INSTR_WIDTH - 5 downto 0) & instr_1(INSTR_WIDTH - 1 downto  0) & instr_2(INSTR_WIDTH - 1 downto INSTR_WIDTH - 8 - 9 - 6-3);
----
----						NOC_BUS_OUT.instr_code <= AGU_instruction; --WRITE
----						NOC_BUS_OUT.bus_enable <= '1';
--						-- this way off making bits the say size is not parameteized and needs to be updated
--						sram_instruction :=unpack_sram_instruction(instr(INSTR_WIDTH -1 downto 0) & instr_1(INSTR_WIDTH - 1 downto 0) & instr_2(INSTR_WIDTH - 1 downto 0));
--						if sram_instruction.sram_end_address_sd = '1' then
--							sram_instruction.End_Address :="0"& raccu_reg_out(CONV_INTEGER(sram_instruction.End_Address(RACCU_REG_ADDRS_WIDTH - 1 downto 0)));
--						end if;
--						if sram_instruction.sram_initial_delay_sd = '1' then
--							raccu_initial_delay_tmp :=raccu_reg_out(CONV_INTEGER(sram_instruction.Initial_Delay(RACCU_REG_ADDRS_WIDTH - 1 downto 0)));
--							sram_instruction.Initial_Delay :=raccu_initial_delay_tmp(INITIAL_DELAY_WIDTH - 1 downto 0); -- this is used to reduce the bits to initial delay level
--						end if;
--						if sram_instruction.sram_middle_delay_sd = '1' then
--							sram_instruction.Middle_Delay :="000"&raccu_reg_out(CONV_INTEGER(sram_instruction.Middle_Delay(RACCU_REG_ADDRS_WIDTH - 1 downto 0)));
--						end if;
--						if sram_instruction.sram_rpt_delay_sd = '1' then
--							sram_instruction.Repetition_Delay :="000"&raccu_reg_out(CONV_INTEGER(sram_instruction.Repetition_Delay(RACCU_REG_ADDRS_WIDTH - 1 downto 0)));
--						end if;
--						if sram_instruction.sram_rpt_incrvalue_sd = '1' then
--							sram_instruction.Increment_Value :="0"&raccu_reg_out(CONV_INTEGER(sram_instruction.Increment_Value(RACCU_REG_ADDRS_WIDTH - 1 downto 0)));
--						end if;
--						if sram_instruction.sram_start_address_sd = '1' then
--							sram_instruction.Start_Address :="0"&raccu_reg_out(CONV_INTEGER(sram_instruction.Start_Address(RACCU_REG_ADDRS_WIDTH - 1 downto 0)));
--						end if;
--						if sram_instruction.sram_nr_of_rpt_sd = '1' then
--							sram_instruction.Number_of_Repetition:= "000"&raccu_reg_out(CONV_INTEGER(sram_instruction.Number_of_Repetition(RACCU_REG_ADDRS_WIDTH - 1 downto 0)));
--						end if;
--						
--						NOC_BUS_OUT <= pack_sram_instruction(sram_instruction);
----						NOC_BUS_OUT <= pack_sram_instruction(unpack_sram_instruction(instr(INSTR_WIDTH - 1 downto 0) & instr_1(INSTR_WIDTH - 1 downto 0) & instr_2(INSTR_WIDTH - 1 downto 0)));
--						non_lin_pc  <= '1';
--						pc_increm_set          <= "11";
--					--					instr_addrs         := instr_addrs + 3;
--					--					agu_instr_en<='1';
--					----------------------------------------------------------------------------------					

					when others =>
						valid_instr <= '0';
				--ASSERT valid_instr = '1' REPORT "Invalid instruction detected. Sequencer will now abort" SEVERITY error;

				end case;
		--END IF;

		end case;
	end process c0;

	reg : process(clk, rst_n)
	begin                               -- PROCESS reg
		if rst_n = '0' then             -- asynchronous reset (active low)
			pres_state <= IDLE_ST;
		elsif clk'event and clk = '1' then -- rising clock edge
			pres_state <= next_state;
		end if;
	end process reg;

end;
