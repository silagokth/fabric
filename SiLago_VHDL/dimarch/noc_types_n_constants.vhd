-------------------------------------------------------
--! @file noc_types_n_constants.vhd
--! @brief 
--! @details 
--! @author 
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
-- File       : noc_types_n_constants.vhd
-- Author     : 
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
-- 
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
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.util_package.ALL;
--USE work.drra_types_n_constants.configwidth;
USE work.top_consts_types_package.ALL;
USE work.tb_instructions.ALL;
USE work.functions.log2c;

PACKAGE noc_types_n_constants IS

    CONSTANT NumberOfRegFile : NATURAL := REG_FILE_DEPTH;
    CONSTANT MAX_ROW         : INTEGER := DiMArch_Rows;
    CONSTANT MAX_COL         : INTEGER := COLUMNS;
    CONSTANT ROW_WIDTH       : INTEGER := DiMArch_Row_Width;
    CONSTANT COL_WIDTH       : INTEGER := INTEGER(log2(NATURAL(MAX_COL)));

    CONSTANT SEQ_ROW : INTEGER := ROWS + 1;
    CONSTANT SEQ_COL : INTEGER := COLUMNS;

    --CONSTANT ROW_WIDTH	: INTEGER := log2c(1+MAX_ROW);
    --CONSTANT COL_WIDTH	: INTEGER := log2c(MAX_COL);

    CONSTANT zero_halt : unsigned (14 DOWNTO 0) := (OTHERS => '0');

    --------------------------------------------------------------------
    -- REGFILE 
    --------------------------------------------------------------------
    --CONSTANT MEM_BLOCK_SIZE : INTEGER := 16;
    --CONSTANT NUM_OF_REG_LOC : INTEGER := NumberOfRegFile/MEM_BLOCK_SIZE;
    --CONSTANT REG_ADDRESS_WIDTH : INTEGER := log2(NUM_OF_REG_LOC);
    --------------------------------------------------------------------
    -- SRAM SEQUENCER 
    --------------------------------------------------------------------
    --CONSTANT SRAM_WIDTH	: INTEGER := MEM_BLOCK_SIZE * BITWIDTH;

    --Constans for SRAM_Sequencer
    -- CONSTANT SRAM_SEQUENCER_INSTRUCTIONS : INTEGER := 16;
    -- CONSTANT SRAM_DEPTH                  : INTEGER := 64;
    -- CONSTANT SRAM_ADDRESS_WIDTH		  : INTEGER := log2_ceil(SRAM_DEPTH);
    -- CONSTANT SRAM_NumberOfInstrRegs      : INTEGER := 256;
    -- CONSTANT SRAM_AGU_INSTR_WIDTH        : INTEGER := ;
    -- CONSTANT SRAM_INSTR_WIDTH            : INTEGER := SRAM_AGU_INSTR_WIDTH+8; 
    CONSTANT RFile_AGU_INSTR_WIDTH : INTEGER := 66;
    CONSTANT INS_WIDTH             : INTEGER := 2;
    -------------------------RFile Signals for MEM Communications----------------------------------
    TYPE reg_INOUT_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF signed (SRAM_WIDTH - 1 DOWNTO 0);
    --------------------------------------------------------------------
    -- SRAM 
    --------------------------------------------------------------------

    TYPE SRAM_RW_address_type IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic_vector (REG_ADDRESS_WIDTH - 1 DOWNTO 0);
    TYPE SRAM_RW_ENABLE_TYPE IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF STD_LOGIC;
    TYPE SRAM_RW_DATA_TYPE IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF STD_LOGIC_VECTOR (SRAM_WIDTH - 1 DOWNTO 0);
    TYPE SRAM_RW_SIGNED_DATA_TYPE IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF SIGNED (SRAM_WIDTH - 1 DOWNTO 0);
    --------------------------------------------------------------------
    -- Segmented Bus 
    --------------------------------------------------------------------

    TYPE NOC_BUS_TYPE IS RECORD
        bus_enable : STD_LOGIC;
        instr_code : std_logic_vector (INS_WIDTH - 1 DOWNTO 0);

        INSTRUCTION : std_logic_vector (NoC_Bus_instr_width - 1 DOWNTO 0);-- ONE EXTRA BIT FOR READ OR WRITE halts will be all zero

    END RECORD;
    CONSTANT ZERO_COL : UNSIGNED (COL_WIDTH - 1 DOWNTO 0) := (OTHERS => '0');
    CONSTANT ZERO_ROW : UNSIGNED (ROW_WIDTH - 1 DOWNTO 0) := (OTHERS => '0');

    CONSTANT IDLE_BUS    : NOC_BUS_TYPE := ('0', (OTHERS => '0'), (OTHERS => '0'));
    CONSTANT NODRIVE_BUS : NOC_BUS_TYPE := ('Z', (OTHERS => 'Z'), (OTHERS => 'Z'));

    --------------------------------------------------------------------
    -- SRAM Sequencer Instruction-Set  
    --------------------------------------------------------------------
    CONSTANT SRAM_inout_select_s     : INTEGER := 0;
    CONSTANT SRAM_inout_select_e     : INTEGER := 0;
    CONSTANT SRAM_inout_select_WIDTH : INTEGER := 1;
    CONSTANT range_counter_s         : INTEGER := 1;
    CONSTANT range_counter_e         : INTEGER := 7;
    CONSTANT range_counter_WIDTH     : INTEGER := 7;
    CONSTANT hault_delay_s           : INTEGER := 8;
    CONSTANT hault_delay_e           : INTEGER := 16;
    CONSTANT hault_delay_WIDTH       : INTEGER := 9;
    CONSTANT hault_counter_s         : INTEGER := 17;
    CONSTANT hault_counter_e         : INTEGER := 22;
    CONSTANT hault_counter_WIDTH     : INTEGER := 6;
    -- NOC DATA
    CONSTANT source_row_address_s : INTEGER := 5;
    CONSTANT source_row_address_e : INTEGER := ROW_WIDTH + source_row_address_s - 1;
    CONSTANT source_col_address_s : INTEGER := source_row_address_e + 1;
    CONSTANT source_col_address_e : INTEGER := COL_WIDTH + source_col_address_s - 1;
    CONSTANT dest_row_address_s   : INTEGER := source_col_address_e + 1;
    CONSTANT dest_row_address_e   : INTEGER := ROW_WIDTH + dest_row_address_s - 1;
    CONSTANT dest_col_address_s   : INTEGER := dest_row_address_e + 1;
    CONSTANT dest_col_address_e   : INTEGER := COL_WIDTH + dest_col_address_s - 1;
    CONSTANT intermediate_l       : INTEGER := dest_col_address_e + 1;
    CONSTANT intr_row_address_s   : INTEGER := intermediate_l + 1;
    CONSTANT intr_row_address_e   : INTEGER := ROW_WIDTH + intr_row_address_s - 1;
    CONSTANT intr_col_address_s   : INTEGER := intr_row_address_e + 1;
    CONSTANT intr_col_address_e   : INTEGER := COL_WIDTH + intr_col_address_s - 1;
    CONSTANT union_l              : INTEGER := intr_col_address_e + 1;
    CONSTANT Origin_l             : INTEGER := union_l + 1;
    CONSTANT RFNODE_s             : INTEGER := Origin_l + 1;
    CONSTANT RFNODE_E             : INTEGER := RFNODE_s + 1;
    CONSTANT RFNODE_l             : INTEGER := Origin_l + 1;
    CONSTANT UNION_PORT_NR_s      : INTEGER := RFNODE_l + 1;
    CONSTANT UNION_PORT_NR_e      : INTEGER := UNION_PORT_NR_s + 1;-- this is not parameterized
    ------------------------------------------------------------------------------------------
    -- INSTRUCTION code 
    ------------------------------------------------------------------------------------------
    -- CONSTANT inst_partition        : UNSIGNED (1 DOWNTO 0) := "00";  -- 9 Partition  
    -- CONSTANT inst_route            : UNSIGNED (1 DOWNTO 0) := "01";  --10 route      
    -- CONSTANT inst_both             : UNSIGNED (1 DOWNTO 0) := "10";  --11 both       
    -- CONSTANT inst_unpartition      : UNSIGNED (1 DOWNTO 0) := "11";  --12 unPartition

    CONSTANT partitioning_instruction : std_logic_vector (INS_WIDTH - 1 DOWNTO 0) := "00";
    CONSTANT route_instruction        : std_logic_vector (INS_WIDTH - 1 DOWNTO 0) := "01";
    CONSTANT raccu_instruction        : std_logic_vector (INS_WIDTH - 1 DOWNTO 0) := "01";
    CONSTANT both_instruction         : std_logic_vector (INS_WIDTH - 1 DOWNTO 0) := "10";
    CONSTANT AGU_instruction          : std_logic_vector (INS_WIDTH - 1 DOWNTO 0) := "11";
    ------------------------------------------------------------------------------------------
    -- INSTRUCTION SWITCH AND ADDRESS DECODERS 
    ------------------------------------------------------------------------------------------
    CONSTANT SR_s                        : INTEGER := 0;
    CONSTANT SR_e                        : INTEGER := ROW_WIDTH + SR_s - 1;
    CONSTANT SC_s                        : INTEGER := SR_e + 1;
    CONSTANT SC_e                        : INTEGER := COL_WIDTH + SC_s - 1;
    CONSTANT DR_s                        : INTEGER := SC_e + 1;
    CONSTANT DR_e                        : INTEGER := ROW_WIDTH + DR_s - 1;
    CONSTANT DC_s                        : INTEGER := DR_e + 1;
    CONSTANT DC_e                        : INTEGER := COL_WIDTH + DC_s - 1;
    CONSTANT IN_l                        : INTEGER := DC_e + 1;
    CONSTANT IR_s                        : INTEGER := IN_l + 1;
    CONSTANT IR_e                        : INTEGER := ROW_WIDTH + IR_s - 1;
    CONSTANT IC_s                        : INTEGER := IR_e + 1;
    CONSTANT IC_e                        : INTEGER := COL_WIDTH + IC_s - 1;
    CONSTANT ON_l                        : INTEGER := IC_e + 1;
    CONSTANT RF_s                        : INTEGER := ON_l + 1;
    CONSTANT RF_e                        : INTEGER := RF_s + 1; -- when south also includes  seqeuncer with rf
    CONSTANT INTERMEDIATE_NODE_FLAG_l    : INTEGER := RF_e + 1;
    CONSTANT INTERMEDIATE_SEGMENT_FLAG_l : INTEGER := INTERMEDIATE_NODE_FLAG_l + 1;
    CONSTANT UNION_FLAG_l                : INTEGER := INTERMEDIATE_SEGMENT_FLAG_l + 1;
    CONSTANT UNION_PORT_s                : INTEGER := UNION_FLAG_l + 1;
    CONSTANT UNION_PORT_e                : INTEGER := UNION_PORT_s + 1;
    CONSTANT READ_WRITE_l                : INTEGER := UNION_PORT_e;
    --CONSTANT RF_l : INTEGER := ON_l+1;
    --CONSTANT 
    --SRC LOCATE
    CONSTANT adjacents : std_logic_vector(3 DOWNTO 0) := "0000";
    CONSTANT Quad_1    : std_logic_vector(3 DOWNTO 0) := "0001";
    CONSTANT Quad_2    : std_logic_vector(3 DOWNTO 0) := "0010";
    CONSTANT Quad_3    : std_logic_vector(3 DOWNTO 0) := "0011";
    CONSTANT Quad_4    : std_logic_vector(3 DOWNTO 0) := "0100";
    CONSTANT Quad_12   : std_logic_vector(3 DOWNTO 0) := "0101";
    CONSTANT Quad_23   : std_logic_vector(3 DOWNTO 0) := "0110";
    CONSTANT Quad_34   : std_logic_vector(3 DOWNTO 0) := "0111";
    CONSTANT Quad_41   : std_logic_vector(3 DOWNTO 0) := "1000";
    -- DST LOCATE
    CONSTANT SAME       : std_logic_vector(3 DOWNTO 0) := "0000";
    CONSTANT NORTH      : std_logic_vector(3 DOWNTO 0) := "0001";
    CONSTANT EAST       : std_logic_vector(3 DOWNTO 0) := "0010";
    CONSTANT WEST       : std_logic_vector(3 DOWNTO 0) := "0011";
    CONSTANT SOUTH      : std_logic_vector(3 DOWNTO 0) := "0100";
    CONSTANT NORTH_EAST : std_logic_vector(3 DOWNTO 0) := "0101";
    CONSTANT NORTH_WEST : std_logic_vector(3 DOWNTO 0) := "0110";
    CONSTANT SOUTH_EAST : std_logic_vector(3 DOWNTO 0) := "0111";
    CONSTANT SOUTH_WEST : std_logic_vector(3 DOWNTO 0) := "1000";
    ------------------------------------------------------------------------------------------
    -- PARTITION HANDLEER  
    ------------------------------------------------------------------------------------------

    TYPE PARTITION_INSTRUCTION_RECORD_TYPE IS RECORD
        ENABLE         : STD_LOGIC;
        PARTITION      : STD_LOGIC_VECTOR (1 DOWNTO 0);
        PRIORITY       : STD_LOGIC;
        reset_PRIORITY : std_logic;

    END RECORD;

    CONSTANT RESET     : STD_LOGIC_VECTOR (1 DOWNTO 0) := "00";
    CONSTANT SET_LEFT  : STD_LOGIC_VECTOR (1 DOWNTO 0) := "01";
    CONSTANT SET_RIGHT : STD_LOGIC_VECTOR (1 DOWNTO 0) := "10";
    CONSTANT SET_BI    : STD_LOGIC_VECTOR (1 DOWNTO 0) := "11";
    CONSTANT SET_UP    : STD_LOGIC_VECTOR (1 DOWNTO 0) := "10";
    CONSTANT SET_DOWN  : STD_LOGIC_VECTOR (1 DOWNTO 0) := "01";

    CONSTANT IDLE_PAR_INST      : PARTITION_INSTRUCTION_RECORD_TYPE := ('0', (OTHERS => '0'), '0', '0');
    CONSTANT LOW_RESET_PAR_INST : PARTITION_INSTRUCTION_RECORD_TYPE := ('1', RESET, '0', '0');

    CONSTANT LOW_UP_PAR_INST   : PARTITION_INSTRUCTION_RECORD_TYPE := ('1', SET_UP, '0', '0');
    CONSTANT LOW_DOWN_PAR_INST : PARTITION_INSTRUCTION_RECORD_TYPE := ('1', SET_DOWN, '0', '0');
    CONSTANT LOW_LEFT_PAR_INST : PARTITION_INSTRUCTION_RECORD_TYPE := ('1', SET_LEFT, '0', '0');
    CONSTANT LOW_RITE_PAR_INST : PARTITION_INSTRUCTION_RECORD_TYPE := ('1', SET_RIGHT, '0', '0');

    CONSTANT HIGH_UP_PAR_INST   : PARTITION_INSTRUCTION_RECORD_TYPE := ('1', SET_UP, '1', '0');
    CONSTANT HIGH_DOWN_PAR_INST : PARTITION_INSTRUCTION_RECORD_TYPE := ('1', SET_DOWN, '1', '0');
    CONSTANT HIGH_LEFT_PAR_INST : PARTITION_INSTRUCTION_RECORD_TYPE := ('1', SET_LEFT, '1', '0');
    CONSTANT HIGH_RITE_PAR_INST : PARTITION_INSTRUCTION_RECORD_TYPE := ('1', SET_RIGHT, '1', '0');

    ------------------------------------------------------------------------------------------
    -- FABRIC 
    ------------------------------------------------------------------------------------------
    TYPE CLK_RST_ARRAY_TYPE IS ARRAY(NATURAL RANGE <>, NATURAL RANGE <>) OF STD_LOGIC;
    TYPE DATA_SIGNAL_TYPE IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF STD_LOGIC_VECTOR (SRAM_WIDTH - 1 DOWNTO 0);
    TYPE DATA_IO_SIGNAL_TYPE IS ARRAY (NATURAL RANGE <>) OF STD_LOGIC_VECTOR (SRAM_WIDTH - 1 DOWNTO 0);
    TYPE DATA_RD_TYPE IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF STD_LOGIC;
    TYPE ADDR_VALID_TYPE IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF STD_LOGIC;
    TYPE INST_SIGNAL_TYPE IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF NOC_BUS_TYPE;
    TYPE NOC_BUS_ARRAY_TYPE IS ARRAY (NATURAL RANGE <>) OF NOC_BUS_TYPE;
    TYPE ROW_ADDR_TYPE IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF UNSIGNED(ROW_WIDTH - 1 DOWNTO 0);
    TYPE COL_ADDR_TYPE IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF UNSIGNED(COL_WIDTH - 1 DOWNTO 0);
    TYPE SEQ_ADDR_RB_ARRAY_TYPE IS ARRAY(NATURAL RANGE <>, NATURAL RANGE <>) OF STD_LOGIC_VECTOR(ROWS - 1 DOWNTO 0);
    TYPE SEQ_ADDR_CB_ARRAY_TYPE IS ARRAY(NATURAL RANGE <>, NATURAL RANGE <>) OF STD_LOGIC_VECTOR(COLUMNS - 1 DOWNTO 0);
    TYPE INSTR_ARRAY_TYPE IS ARRAY(NATURAL RANGE <>, NATURAL RANGE <>) OF STD_LOGIC_VECTOR(INSTR_WIDTH - 1 DOWNTO 0);

    TYPE SRAM_ADDR_TYPE IS ARRAY(NATURAL RANGE <>, NATURAL RANGE <>) OF STD_LOGIC_VECTOR(SRAM_ADDRESS_WIDTH - 1 DOWNTO 0);
    TYPE SRAM_ROW_TYPE IS ARRAY(NATURAL RANGE <>, NATURAL RANGE <>) OF UNSIGNED(ROW_WIDTH - 1 DOWNTO 0);
    TYPE SRAM_COL_TYPE IS ARRAY(NATURAL RANGE <>, NATURAL RANGE <>) OF UNSIGNED(COL_WIDTH - 1 DOWNTO 0);

    TYPE PRIORITY_TYPE IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF STD_LOGIC;
    TYPE DIRECTION_TYPE IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF STD_LOGIC_VECTOR (1 DOWNTO 0);

    TYPE PARTITION_INSTRUCTION_TYPE IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF PARTITION_INSTRUCTION_RECORD_TYPE;
    TYPE PARTITION_INSTRUCTION_STATUS_TYPE IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic_vector (1 DOWNTO 0);

    CONSTANT zero_block : STD_LOGIC_VECTOR (SRAM_WIDTH - 1 DOWNTO 0) := (OTHERS => '0');

    ------------------------------------------------------------------------------------------
    -- FABRIC 
    ------------------------------------------------------------------------------------------

    TYPE LOADER_INSTRUCTION_RECORD_TYPE IS RECORD
        ENABLE           : STD_LOGIC;
        initial_delay    : STD_LOGIC_VECTOR(INITIAL_DELAY_WIDTH - 1 DOWNTO 0);
        iterations       : STD_LOGIC_VECTOR(SRAM_ADDRESS_WIDTH - 1 DOWNTO 0);
        repitition_delay : STD_LOGIC_VECTOR(INITIAL_DELAY_WIDTH - 1 DOWNTO 0);
        direction        : STD_LOGIC_VECTOR(1 DOWNTO 0);
        seq_sel          : std_logic_vector(ROWS DOWNTO 0);
    END RECORD;

    CONSTANT FROM_LCC_TO_CROSSBAR : STD_LOGIC_VECTOR (1 DOWNTO 0) := "00";
    CONSTANT FROM_CROSSBAR_TO_LCC : STD_LOGIC_VECTOR (1 DOWNTO 0) := "01";
    CONSTANT FROM_CROSSBAR_TO_SEQ : STD_LOGIC_VECTOR (1 DOWNTO 0) := "10";
    CONSTANT reset_direction      : STD_LOGIC_VECTOR (1 DOWNTO 0) := "11";-- unused

    TYPE LOADER_ARRAY_TYPE IS ARRAY(NATURAL RANGE <>) OF LOADER_INSTRUCTION_RECORD_TYPE;

    CONSTANT ZERO_DELAY     : STD_LOGIC_VECTOR (INITIAL_DELAY_WIDTH - 1 DOWNTO 0) := (OTHERS => '0');
    CONSTANT ONE_DELAY      : STD_LOGIC_VECTOR (INITIAL_DELAY_WIDTH - 1 DOWNTO 0) := std_logic_vector(to_unsigned(1, INITIAL_DELAY_WIDTH));
    CONSTANT NO_LOAD        : std_logic_vector (3 DOWNTO 0)                       := "0000";
    CONSTANT INST_BLOCK     : STD_LOGIC_VECTOR (SRAM_ADDRESS_WIDTH - 1 DOWNTO 0)  := std_logic_vector(to_unsigned(4, SRAM_ADDRESS_WIDTH));
    CONSTANT ONE_ITIRATION  : STD_LOGIC_VECTOR (SRAM_ADDRESS_WIDTH - 1 DOWNTO 0)  := std_logic_vector(to_unsigned(1, SRAM_ADDRESS_WIDTH));
    CONSTANT ZERO_ITIRATION : STD_LOGIC_VECTOR (SRAM_ADDRESS_WIDTH - 1 DOWNTO 0)  := std_logic_vector(to_unsigned(0, SRAM_ADDRESS_WIDTH));
    CONSTANT Sm             : std_logic_vector (3 DOWNTO 0)                       := "0001";
    CONSTANT S0             : std_logic_vector (3 DOWNTO 0)                       := "0010";
    CONSTANT S1             : std_logic_vector (3 DOWNTO 0)                       := "0100";
    CONSTANT Sc             : std_logic_vector (3 DOWNTO 0)                       := "1000";
    CONSTANT Sm_n_S0        : std_logic_vector (3 DOWNTO 0)                       := "0011";
    CONSTANT Sm_n_S1        : std_logic_vector (3 DOWNTO 0)                       := "0101";
    CONSTANT Sm_n_Sc        : std_logic_vector (3 DOWNTO 0)                       := "1001";

    CONSTANT S0_n_S1 : std_logic_vector (3 DOWNTO 0) := "0110";
    CONSTANT S0_n_Sc : std_logic_vector (3 DOWNTO 0) := "1010";

    CONSTANT S1_n_Sc : std_logic_vector (3 DOWNTO 0) := "1100";

    CONSTANT Sm_n_S0_S1 : std_logic_vector (3 DOWNTO 0) := "0111";
    CONSTANT Sm_n_S0_Sc : std_logic_vector (3 DOWNTO 0) := "1011";
    CONSTANT Sm_n_S1_Sc : std_logic_vector (3 DOWNTO 0) := "1101";

    CONSTANT Sm_S0_S1_Sc : std_logic_vector (3 DOWNTO 0) := "1111";
    --ALIAS i_instr_code                   : UNSIGNED (3 downto 0) IS instr(SRAM_INSTR_WIDTH-1 DOWNTO SRAM_INSTR_WIDTH-4);
    CONSTANT LD_I_E : INTEGER := 5;
    CONSTANT LD_I_S : INTEGER := LD_I_E + INITIAL_DELAY_WIDTH - 1;
    CONSTANT LD_N_E : INTEGER := LD_I_S + 1;
    CONSTANT LD_N_S : INTEGER := LD_N_E + SRAM_ADDRESS_WIDTH - 1;
    CONSTANT LD_F_E : INTEGER := LD_N_S + 1;
    CONSTANT LD_F_S : INTEGER := LD_F_E + INITIAL_DELAY_WIDTH - 1;
    CONSTANT LD_S_E : INTEGER := LD_F_S + 1;
    CONSTANT LD_S_S : INTEGER := LD_S_E + ROWS;
    CONSTANT LD_D_E : INTEGER := LD_S_S + 1;
    CONSTANT LD_D_S : INTEGER := LD_D_E + 1;

    ---------------------------------------------------------------- NEW SRAM _AGU-------------------------------------------
    TYPE sram_agu_instruction_type IS RECORD
        enable          : std_logic;
        agu_mode        : std_logic;-- 0 for liniar 1 for FFT
        rw              : std_logic;
        Initial_Address : unsigned(sr_initial_address_width - 1 DOWNTO 0);

        Loop1_interations : unsigned(sr_loop1_iteration_width - 1 DOWNTO 0);-- burst:mode
        Loop1_increment   : signed (sr_loop1_increment_width - 1 DOWNTO 0); -- power of two value

        Loop2_iterations : UNSIGNED(sr_loop2_iteration_width - 1 DOWNTO 0);
        Loop2_increment  : signed(sr_loop2_increment_width - 1 DOWNTO 0);

        Initial_Delay : UNSIGNED(INITIAL_DELAY_WIDTH - 1 DOWNTO 0);
        Loop1_Delay   : UNSIGNED(sr_loop1_delay_width - 1 DOWNTO 0);
        Loop2_Delay   : UNSIGNED(sr_loop2_delay_width - 1 DOWNTO 0);
    END RECORD;
    --CONSTANT ZERO_INITIAL_DELAY : UNSIGNED(INITIAL_DELAY_WIDTH - 1 downto 0);		
    CONSTANT sram_instr_zero : sram_agu_instruction_type :=
    (
    '0',             --enable            
    '0',             --agu_mode          
    '0',             -- rw
    (OTHERS => '0'), --Initial_Address   
    --                  
    (OTHERS => '0'), --Loop1_interations 
    (OTHERS => '0'), --Loop1_increment   
    --                  
    (OTHERS => '0'), --Loop2_iterations  
    (OTHERS => '0'), --Loop2_increment   
    --                  
    (OTHERS => '0'), --Initial_Delay     
    (OTHERS => '0'), --Loop1_Delay       
    (OTHERS => '0')--Loop2_Delay       

    );
    ---------------------------------------------------------------- OLD SRAM _AGU-------------------------------------------
    --TYPE sram_agu_type is record
    ----		new_instruction      : std_logic;
    --		new_instruction      : std_logic;--(0 downto 0);
    --		agu_mode             : std_logic;-- (0 downto 0);
    --		Start_Address        : UNSIGNED(start_addrs_WIDTH - 1 downto 0);
    --		End_Address          : UNSIGNED(end_addrs_WIDTH - 1 downto 0);
    --		Increment            : std_logic;
    --		Increment_Value      : UNSIGNED(incr_decr_value_WIDTH - 1 downto 0);
    --		Initial_Delay        : UNSIGNED(INITIAL_DELAY_WIDTH - 1 downto 0);
    --		Output_Control       : UNSIGNED (outputcontrol_WIDTH-1 downto 0);
    --		Number_of_Repetition : UNSIGNED(repetition_delay_WIDTH - 1 downto 0);
    --		Repetition_Delay     : UNSIGNED(repetition_delay_WIDTH - 1 downto 0);
    --		Repetition_Increment : std_logic;
    --		Repetition_Value     : UNSIGNED(repetition_incr_decr_value_WIDTH - 1 downto 0);
    --		Middle_Delay         : UNSIGNED(middle_delay_WIDTH - 1 downto 0);
    --		Range_counter        : UNSIGNED(range_counter_WIDTH - 1 downto 0);
    --		hault_delay          : UNSIGNED (hault_delay_WIDTH-1 downto 0); 
    --		Hault_Counter        : UNSIGNED(hault_counter_WIDTH-1 downto 0);
    --		Infinite_loop        : std_logic;
    --		start_stage          : UNSIGNED(start_stage_WIDTH-1 downto 0);
    --		end_stage            : UNSIGNED (end_stage_WIDTH-1 downto 0); 
    --
    --	end record;
    --
    --	constant agu_zero : sram_agu_type :=(
    --		'0',
    --		'0',
    --		(others  => '0'),
    --		(others  => '0'),
    --		 '0',
    --		(others  => '0'),
    --		(others  => '0'),
    --		(others  => '0'),
    --		(others  => '0'),
    --		(others  => '0'),
    --		'0',
    --		(others  => '0'),
    --		(others  => '0'),
    --		(others  => '0'),
    --		(others  => '0'),
    --		(others  => '0'),
    --		'0',
    --		(others  => '0'),
    --		(others  => '0')
    --		
    --	);
    --	

    FUNCTION unpack_sram_noc_agu(arg : std_logic_vector(NoC_Bus_instr_width - 1 DOWNTO 0))RETURN sram_agu_instruction_type;
    --Function unpack_sram_agu(arg     :std_logic_vector(NoC_Bus_instr_width-1 DOWNTO 0))RETURN sram_agu_type;
END PACKAGE noc_types_n_constants;

PACKAGE BODY noc_types_n_constants IS

    FUNCTION unpack_sram_noc_agu(arg : std_logic_vector(NoC_Bus_instr_width - 1 DOWNTO 0))RETURN sram_agu_instruction_type IS
        VARIABLE result                  : sram_agu_instruction_type;

    BEGIN
        result.enable          := arg(NoC_Bus_instr_width - sr_en_s);
        result.agu_mode        := arg(NoC_Bus_instr_width - sr_mode_s);
        result.Initial_Address := unsigned(arg(NoC_Bus_instr_width - sr_initial_address_s DOWNTO NoC_Bus_instr_width - sr_initial_address_e));
        result.Initial_Delay   := unsigned(arg(NoC_Bus_instr_width - sr_initial_delay_s DOWNTO NoC_Bus_instr_width - sr_initial_delay_e));

        result.Loop1_interations := unsigned(arg(NoC_Bus_instr_width - sr_loop1_iteration_s DOWNTO NoC_Bus_instr_width - sr_loop1_iteration_e));
        result.Loop1_increment   := signed(arg(NoC_Bus_instr_width - sr_loop1_increment_s DOWNTO NoC_Bus_instr_width - sr_loop1_increment_e));
        result.Loop1_Delay       := unsigned(arg(NoC_Bus_instr_width - sr_loop1_delay_s DOWNTO NoC_Bus_instr_width - sr_loop1_delay_e));

        result.Loop2_iterations := unsigned(arg(NoC_Bus_instr_width - sr_loop2_iteration_s DOWNTO NoC_Bus_instr_width - sr_loop2_iteration_e));
        result.Loop2_increment  := signed(arg(NoC_Bus_instr_width - sr_loop2_increment_s DOWNTO NoC_Bus_instr_width - sr_loop2_increment_e));
        result.Loop2_Delay      := unsigned(arg(NoC_Bus_instr_width - sr_loop2_delay_s DOWNTO NoC_Bus_instr_width - sr_loop2_delay_e));
        result.rw               := arg(NoC_Bus_instr_width - sr_rw);

        RETURN result;
    END;
    --Function unpack_sram_agu(arg     :std_logic_vector(NoC_Bus_instr_width-1 DOWNTO 0))RETURN sram_agu_type;	
    --
    --	
    --	Function unpack_sram_agu(arg     :std_logic_vector(NoC_Bus_instr_width-1 DOWNTO 0))RETURN sram_agu_type IS 
    --	variable result : sram_agu_type;
    --	
    --BEGIN
    ----		result.instr_code          :='1'; 
    --		result.new_instruction     :=         arg(NoC_Bus_instr_width -new_instr_s);--                downto NoC_Bus_instr_width-new_instr_e  );
    --		result.agu_mode            :=         arg(NoC_Bus_instr_width -mode_s);--                        downto NoC_Bus_instr_width-mode_e          );
    --		result.Start_Address       :=UNSIGNED(arg(NoC_Bus_instr_width -start_addrs_s                 downto NoC_Bus_instr_width-start_addrs_e               ));--61 DOWNTO 55
    --		result.End_Address         :=UNSIGNED(arg(NoC_Bus_instr_width -end_addrs_s                   downto NoC_Bus_instr_width-end_addrs_e                 ));--54 DOWNTO 48
    --		result.Increment           :=         arg(NoC_Bus_instr_width -incr_decr_s);--               downto NoC_Bus_instr_width-incr_decr_e                 )); 47
    --		result.Increment_Value     :=UNSIGNED(arg(NoC_Bus_instr_width -incr_decr_value_s             downto NoC_Bus_instr_width-incr_decr_value_e           )); --46 DOWNTO 40
    --		result.Initial_Delay       :=UNSIGNED(arg(NoC_Bus_instr_width -initial_delay_s               downto NoC_Bus_instr_width-initial_delay_e             ));--39 DOWNTO 
    --		result.Output_Control      :=UNSIGNED(arg(NoC_Bus_instr_width -outputcontrol_s               downto NoC_Bus_instr_width-outputcontrol_e             ));
    --		result.Infinite_loop       :=         arg(NoC_Bus_instr_width -infinite_loop_s);--               downto NoC_Bus_instr_width-infinite_loop_e             ));
    --		result.Repetition_Delay    :=UNSIGNED(arg(NoC_Bus_instr_width -repetition_delay_s            downto NoC_Bus_instr_width-repetition_delay_e          ));
    --		result.Number_of_Repetition:=UNSIGNED(arg(NoC_Bus_instr_width -no_of_repetitions_s           downto NoC_Bus_instr_width-no_of_repetitions_e         ));
    --		result.Repetition_Increment:=         arg(NoC_Bus_instr_width -repetition_incr_decr_s);--    downto NoC_Bus_instr_width-repetition_incr_decr_e      ));
    --		result.Repetition_Value    :=UNSIGNED(arg(NoC_Bus_instr_width -repetition_incr_decr_value_s  downto NoC_Bus_instr_width-repetition_incr_decr_value_e));
    --		result.Middle_Delay        :=UNSIGNED(arg(NoC_Bus_instr_width -middle_delay_s                downto NoC_Bus_instr_width-middle_delay_e              ));
    --		result.Range_counter       :=(others => '0');
    --		result.Hault_Counter       :=(others => '0');
    --		result.hault_delay         := (others => '0');
    --		result.start_stage := (others  => '0');
    --		result.end_stage := (others  => '0');
    --	RETURN result;
    --END;

END PACKAGE BODY noc_types_n_constants;