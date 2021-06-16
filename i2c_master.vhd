----------------------------------------------------------------------------------
-- Name: Daniel Hopfinger
-- Date: 08.06.2021
-- Module: i2c_master
-- Description:
-- I2C Master module to initiate I2C transmissions.
--
-- History:
-- Version  | Date       | Information
-- ----------------------------------------
--  0.0.1   | 08.06.2021 | Initial version.
--
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use ieee.numeric_std.all;


entity i2c_master is
    generic (
        G_SYSTEM_CLOCK   : integer range 5000000 to 400000000 := 100000000;    -- max: 400 Mhz, min: 5 Mhz
        G_BAUD_RATE      : integer range 100000 to 1000000 := 400000           -- max: 1000000 bit/s, min: 50 bit/s. Maximum only with sys_clk > 6 MHz
    );
    port (
        i_sys_clk : in std_logic;                               -- system clock
        i_sys_rst : in std_logic;                               -- system reset

        -- I2C interface
        o_scl    : out std_logic;                               -- I2C clock output
        i_scl    : in  std_logic;                               -- I2C clock input
        t_scl    : out std_logic;                               -- I2C clock trisate
        o_sda    : out std_logic;                               -- I2C data output
        i_sda    : in  std_logic;                               -- I2C data input
        t_sda    : out std_logic;                               -- I2C data tristate

        -- General interface
        i_en        : in  std_logic;                            -- Starts a new i2c transfer
        o_busy      : out std_logic;                            -- Busy as long as module is not in idle state
        o_ack_error : out std_logic;                            -- Register of last transfer. Asserted if last transfer was error.
        i_stop_mode : in  std_logic_vector(1 downto 0);         -- Mode pin to set possible repeated start and no end

        -- Write interface
        i_wr_data   : in  std_logic_vector(7 downto 0);         -- Data to be written to slave.
        i_wr_en     : in  std_logic;                            -- Enable puls. If asserted next data is written to slave.
        o_sending   : out std_logic;                            -- Status signal to indicate if sending is done. If done then assert wr_en for next data.

        -- Read interface
        o_rd_data   : out std_logic_vector(7 downto 0);         -- Output of data read from slave
        i_rd_en     : in  std_logic;                            -- Enable puls to generate ack to slave and start next read phase.
        o_receiving : out std_logic                             -- Status signal to indicate when receiving is in progress. If deasserted rd_data can be taken and rd_en pulsed for next read phase
    );
end i2c_master;

architecture rtl of i2c_master is

    constant C_BIT_PERIOD           : unsigned(15 downto 0) := to_unsigned((G_SYSTEM_CLOCK / G_BAUD_RATE), 16);
    constant C_ZERO_PERIOD          : unsigned(15 downto 0) := x"0003";
    constant C_ONE_QUARTER_PERIOD   : unsigned(15 downto 0) := "00" & C_BIT_PERIOD(15 downto 2);
    constant C_HALF_PERIOD          : unsigned(15 downto 0) := '0' & C_BIT_PERIOD(15 downto 1);
    constant C_THREE_QUARTER_PERIOD : unsigned(15 downto 0) := C_HALF_PERIOD + C_ONE_QUARTER_PERIOD;

    type i2c_fsm is (   init,               --! Sets signals to inital values.
                        idle,               --! Idles until i_en gets asserted. 
                        start_cond,         --! Generates a start condition on i2c bus.
                        get_send_data,      --! Waits for user logic to write data that is send on the bus.
                        send_data,          --! Sends data over bus.
                        get_ack,            --! Get ack/nack from slave and decide accordingly.
                        receive_data,       --! Receives data from bus. 
                        get_receive_data,   --! Waits for user logic to confirm if another data is read from the bus or stop condition is issued.
                        send_ack,           --! Sends ack or nack to slave.
                        stop_cond,          --! Sends a stop condition on i2c bus.
                        repstart_cond );    --! Sends a repeated start condition on i2c bus.
    signal i2c_state : i2c_fsm;

    signal start_alert      : std_logic;    --! Signal for bus arbitration.
    signal start_detected   : std_logic;    --! Signal for bus arbitration.
    signal stop_alert       : std_logic;    --! Signal for bus arbitration.
    signal stop_detected    : std_logic;    --! Signal for bus arbitration.

    signal scl_out     : std_logic;         --! Output signal for SCL line.
    signal scl_in      : std_logic;         --! Input signal for SCL line.
    signal sda_out     : std_logic;         --! Output signal for SDA line.
    signal sda_in      : std_logic;         --! Input signal for SDA line.
    signal scl_tri     : std_logic;         --! Tristate signal for SCL line.
    signal sda_tri     : std_logic;         --! Tristate signal for SDA line.

    signal sda_r1      : std_logic;        --! Register for SDA line. Used for bus arbitration.
    signal scl_r1      : std_logic;        --! Register for SCL line. Used for bus arbitration.

    signal en_r1        : std_logic;        --! Register for i_en.
    signal busy         : std_logic;        --! Busy signal indicating when start condition was detected (For both this master and other master on the bus)
    signal ack_error    : std_logic;        --! Error status if nack is detected.
    signal sending      : std_logic;        --! Status signal indicating byte is being sent.
    signal rd_data_reg  : std_logic_vector(7 downto 0); --! Data that was read from the bus.
    signal wr_data_reg  : std_logic_vector(7 downto 0); --! Data that is sent on the bus.
    signal receiving    : std_logic;        --! Status signal indicating a byte is received.
    signal receive_done : std_logic;        --! Status bit indicating for FSM if another byte should be read from bus.
    signal ack_nack     : std_logic;        --! Status signal for ack or nack.

    --! Counter that generates the pulse signals.
    --! One bit period is divided into 5 pulses (0 to 4). Pulse 0 and 4 are beginning and end of Period.
    --! Each pulse is used to change level of SCL and SDA lines.
    signal cnt_en           : std_logic;    --! Enables the counter
    signal cnt_en_r1        : std_logic;    --! Register for en
    signal halt_cnt         : std_logic;    --! Stops the counter. If user logic input is expected then the counter is halted until input is presented.
    signal cnt              : unsigned(15 downto 0); --! Counter
    signal cnt_zero_pulse   : std_logic;    --! Pulse when bit period starts.
    signal cnt_one_pulse    : std_logic;    --! Pulse when 1/4 of bit period is reached.
    signal cnt_two_pulse    : std_logic;    --! Pulse when 2/4 of bit period is reached.
    signal cnt_three_pulse  : std_logic;    --! Pulse when 3/4 of bit period is reached.
    signal cnt_four_pulse   : std_logic;    --! Pulse when 4/4 of bit period is reached.

    
    signal stop_mode_reg    : std_logic_vector(1 downto 0); --! Register for stop_mode input port
    signal first_byte       : std_logic;                    --! Status signal indicating the first byte being sent on the bus. First byte is for slave address and RW flag.
    signal rw_mode          : std_logic;                    --! Register for rw mode. Bit 0 of first byte.
    signal bit_cnt          : unsigned(3 downto 0);         --! Bit counter. Counts from 7 to 0.

begin

    o_busy      <= busy;
    o_ack_error <= ack_error;
    o_sending   <= sending;
    o_rd_data   <= rd_data_reg;
    o_receiving <= receiving;

    --! Insert following line to top module level to infer tristate buffer
    --io_scl <= '0' when scl_tri = '0' else 'Z';
    scl_in <= i_scl;
    o_scl  <= scl_out;
    t_scl  <= scl_tri;

    --! Insert following line to top module level to infer tristate buffer
    --! io_sda <= '0' when sda_tri = '0' else 'Z';
    sda_in <= i_sda;
    o_sda  <= sda_out;
    t_sda  <= sda_tri;

    busy <= '1' when start_detected = '1' or start_alert = '1' else '0';

    --! Bus arbitration process. Monitors SCL and SDA lines if start condition is detected.
    --! If start detection is detectd asserts busy signal. Stop condition deasserts it again.
    --! Detects start/stop conditions from all masters on the bus.
    bus_arb_proc: process (i_sys_clk)
    begin
        if rising_edge(i_sys_clk) then
            if i_sys_rst = '1' then
                start_alert     <= '0';
                start_detected  <= '0';
                stop_alert      <= '0';
                stop_detected   <= '1';

            else
                sda_r1 <= sda_in;
                scl_r1 <= scl_in;

                if stop_detected = '1' then
                    if sda_in = '0' and sda_r1 = '1' then
                        start_alert <= '1';
                    elsif sda_in = '1' and sda_r1 = '0' then
                        start_alert <= '0';
                    end if;
                end if;

                if start_alert = '1' then
                    if scl_in = '0' and scl_r1 = '1' then
                        start_alert <= '0';
                        start_detected <= '1';
                        stop_detected <= '0';
                    end if;
                end if;

                if start_detected = '1' then
                    if scl_in = '1' and scl_r1 = '0' then
                        stop_alert <= '1';
                    elsif scl_in = '0' and scl_r1 = '1' then
                        stop_alert <= '0';
                    end if;
                end if;

                if stop_alert = '1' then
                    if sda_in = '1' and sda_r1 = '0' then
                        stop_alert <= '0';
                        start_detected <= '0';
                        stop_detected <= '1';
                    end if;
                end if;

            end if;
        end if;
    end process bus_arb_proc;

    --! Actuall FSM für I2C bus handling.
    i2c_proc : process (i_sys_clk)
    begin
        if rising_edge(i_sys_clk) then
            if i_sys_rst = '1' then
                i2c_state   <= init;
                cnt_en      <= '0';
                halt_cnt    <= '0';
                sda_tri     <= '1';
                sda_out     <= '1';
                scl_tri     <= '1';
                scl_out     <= '1';
                ack_error   <= '0';
                sending     <= '0';
                receiving   <= '0';
                rd_data_reg <= (others => '0');
            else
                en_r1 <= i_en;

                case i2c_state is
                    when init =>
                        cnt_en      <= '0';
                        halt_cnt    <= '0';
                        sda_tri     <= '1';
                        scl_tri     <= '1';
                        sda_tri     <= '1';
                        sda_out     <= '1';
                        scl_tri     <= '1';
                        scl_out     <= '1';
                        ack_error   <= '0';
                        sending     <= '0';
                        receiving   <= '0';
                        rd_data_reg <= (others => '0');

                        i2c_state   <= idle;

                    when idle =>
                        if i_en = '1' and busy = '0' then
                            ack_error     <= '0';
                            stop_mode_reg <= i_stop_mode;
                            bit_cnt       <= x"7";
                            
                            cnt_en    <= '1';
                            scl_tri   <= '0';
                            sda_tri   <= '0';
                            i2c_state <= start_cond;
                        end if;

                    when start_cond =>
                        if cnt_one_pulse = '1' then
                            sda_out <= '0';
                        end if;

                        if cnt_two_pulse = '1' then
                            scl_out <= '0';
                        end if;

                        if cnt_four_pulse = '1' then
                            first_byte <= '1';
                            halt_cnt   <= '1';
                            i2c_state  <= get_send_data;

                            if en_r1 = '0' then
                                halt_cnt <= '0';
                                i2c_state <= stop_cond;
                            end if;
                        end if;

                    when get_ack =>
                        if cnt_one_pulse = '1' then
                            scl_out <= '1';
                        end if;

                        if cnt_two_pulse = '1' then
                            if sda_in = '0' then
                                ack_nack <= '0';
                            else
                                ack_nack <= '1';
                            end if;
                        end if;
                        
                        if cnt_three_pulse = '1' then
                            scl_out <= '0';
                        end if;

                        if cnt_four_pulse = '1' then
                            sending   <= '0';
                            if ack_nack = '0' then
                                if rw_mode = '1' then
                                    receiving <= '1';
                                    i2c_state <= receive_data;
                                else
                                    halt_cnt  <= '1';
                                    sda_tri   <= '0';
                                    sda_out   <= '0';
                                    i2c_state <= get_send_data;
                                end if;
                                bit_cnt <= x"7";
                            else
                                ack_error <= '1';
                                sda_tri   <= '0';
                                sda_out   <= '1';
                                i2c_state <= stop_cond;
                            end if;

                            if en_r1 = '0' then
                                sda_tri   <= '0';
                                sda_out   <= '0';
                                i2c_state <= stop_cond;
                            end if;
                        end if;

                    when get_send_data =>
                        if i_wr_en = '1' then
                            wr_data_reg <= i_wr_data;
                            if first_byte = '1' then
                                first_byte <= '0';
                                rw_mode    <= i_wr_data(0);
                            end if;
                            sending     <= '1';
                            halt_cnt    <= '0';
                            bit_cnt     <= x"7";
                            i2c_state   <= send_data;
                        end if;

                        if en_r1 = '0' then
                            halt_cnt  <= '0';

                            if stop_mode_reg = "00" then
                                i2c_state <= stop_cond;
                            elsif stop_mode_reg = "01" then
                                i2c_state <= repstart_cond;
                            else 
                                i2c_state <= stop_cond;
                            end if;
                        end if;
                    
                    when send_data =>
                        if cnt_zero_pulse = '1' then
                            sda_out <= wr_data_reg(to_integer(bit_cnt));
                        end if;

                        if cnt_one_pulse = '1' then
                            scl_out <= '1';
                        end if;

                        if cnt_three_pulse = '1' then
                            scl_out <= '0';
                        end if;

                        if cnt_four_pulse = '1' then
                            bit_cnt <= bit_cnt - 1;
                            if bit_cnt = x"0" then
                                sda_tri   <= '1';
                                i2c_state <= get_ack;
                            end if;

                            if en_r1 = '0' then
                                i2c_state <= stop_cond;
                            end if;
                        end if;

                    when receive_data =>
                        if cnt_one_pulse = '1' then
                            scl_out <= '1';
                        end if;

                        if cnt_two_pulse = '1' then
                            rd_data_reg(to_integer(bit_cnt)) <= sda_in;
                        end if;

                        if cnt_three_pulse = '1' then
                            scl_out <= '0';
                        end if;

                        if cnt_four_pulse = '1' then
                            bit_cnt <= bit_cnt - 1;
                            if bit_cnt = x"0" then
                                halt_cnt <= '1';
                                receiving <= '0';
                                i2c_state <= get_receive_data;
                            end if;

                            if en_r1 = '0' then
                                sda_tri   <= '0';
                                sda_out   <= '0';
                                i2c_state <= stop_cond;
                            end if;
                        end if;

                    when get_receive_data =>
                        if i_rd_en = '1' then
                            receive_done <= '0';
                            halt_cnt     <= '0';
                            sda_tri      <= '0';
                            sda_out      <= '0';
                            i2c_state    <= send_ack;
                        end if;

                        if en_r1 = '0' then
                            receive_done <= '1';
                            halt_cnt     <= '0';
                            sda_tri      <= '0';
                            sda_out      <= '1';
                            i2c_state    <= send_ack;
                        end if;

                    when send_ack =>
                        if cnt_one_pulse = '1' then
                            scl_out <= '1';
                        end if;

                        if cnt_three_pulse = '1' then
                            scl_out <= '0';
                        end if;

                        if cnt_four_pulse = '1' then

                            if receive_done = '1' then
                                if stop_mode_reg = "00" then
                                    i2c_state <= stop_cond;
                                elsif stop_mode_reg = "01" then
                                    i2c_state <= repstart_cond;
                                else
                                    i2c_state <= stop_cond;
                                end if;
                            else
                                sda_tri   <= '1';
                                sda_out   <= '1';
                                receiving <= '1';
                                bit_cnt   <= x"7";
                                i2c_state <= receive_data;
                            end if;

                            if en_r1 = '0' then
                                i2c_state <= stop_cond;
                            end if;
                        end if;
                    
                    when stop_cond =>
                        if cnt_zero_pulse = '1' then
                            sda_out <= '0';
                        end if;

                        if cnt_two_pulse = '1' then
                            scl_out <= '1';
                        end if;

                        if cnt_three_pulse = '1' then
                            sda_out <= '1';
                        end if;

                        if cnt_four_pulse = '1' then
                            scl_tri   <= '1';
                            sda_tri   <= '1';
                            cnt_en    <= '0';
                            i2c_state <= idle;
                        end if;

                    when repstart_cond =>
                        if cnt_zero_pulse = '1' then
                            sda_out <= '1';
                        end if;

                        if cnt_two_pulse = '1' then
                            scl_out <= '1';
                        end if;

                        if cnt_four_pulse = '1' then
                            ack_error     <= '0';
                            stop_mode_reg <= i_stop_mode;
                            i2c_state     <= start_cond;
                        end if;

                    when others =>
                        null;
                end case;


            end if;
        end if;
    end process i2c_proc;

    --! Counter that generates the pulses to sync toggling of SCL and SDA lines.
    cnt_proc : process (i_sys_clk)
    begin
        if rising_edge(i_sys_clk) then
            if i_sys_rst = '1' then
                cnt             <= (others => '0');
                cnt_zero_pulse  <= '0';
                cnt_one_pulse   <= '0';
                cnt_two_pulse   <= '0';
                cnt_three_pulse <= '0';
                cnt_four_pulse  <= '0';
            else
                cnt_zero_pulse  <= '0';
                cnt_one_pulse   <= '0';
                cnt_two_pulse   <= '0';
                cnt_three_pulse <= '0';
                cnt_four_pulse  <= '0';

                cnt_en_r1 <= cnt_en;
                if cnt_en = '1' and cnt_en_r1 = '0' then
                    cnt <= (others => '0');
                end if;

                if cnt_en_r1 = '1' and halt_cnt <= '0' then
                    cnt <= cnt + 1;

                    if cnt = C_ZERO_PERIOD - 1 then
                        cnt_zero_pulse <= '1';
                    end if; 

                    if cnt = C_ONE_QUARTER_PERIOD - 1 then
                        cnt_one_pulse <= '1';
                    end if; 

                    if cnt = C_HALF_PERIOD - 1 then
                        cnt_two_pulse <= '1';
                    end if; 

                    if cnt = C_THREE_QUARTER_PERIOD - 1 then
                        cnt_three_pulse <= '1';
                    end if; 

                    if cnt = C_BIT_PERIOD - 1 then
                        cnt_four_pulse <= '1';
                        cnt <= (others => '0');
                    end if;

                end if;

            end if;
        end if;
    end process cnt_proc;

end rtl;