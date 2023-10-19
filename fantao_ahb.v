module ahb_sram_interface # (parameter AW =8,parameter DW =32) (
//ahb
    input               hclk,
    input               hrst_n,
    input   [DW-1:0]    hwdata,
    input   [AW-1:0]    haddr,
    input               hwrite,
    input   [1:0]       htrans,
    input               hsize,
    input               hburst,
    input               hsel,
    input               hprot,

    output              hready,
    output  [DW-1:0]    hrdata,
    output              hresp,
//sram
    input               clk_sram,
    input               rst_sram
);

    wire    [DW-1:0]    fifo_data_in;
    wire    [DW-1:0]    fifo_addr_in;
    wire                fifo_wr_en;
    wire                fifo_addr_en;
    wire    [DW-1:0]    fifo_data_out;
    wire    [DW-1:0]    fifo_addr_out;
    wire    [DW-1:0]    fifo_read_data_in;
    wire    [DW-1:0]    fifo_read_data_out;
    wire                read_fifo_wr_en;
    wire                addr_fifo_rd_en;
    wire                fifo_full;
    wire                read_fifo_empty;
    wire                addr_fifo_empty;
 
    assign read_fifo_rd_en = !read_fifo_empty;
    assign addr_fifo_rd_en = !addr_fifo_empty;

    /*********************************AHB******************************/
    wire                hrw_en;
    wire                hwrite_en;
    wire                hread_en;
    wire    [AW:0]      haddr_wr;
    reg     [AW:0]      haddr_sl;
    reg                 hwrite_en_reg;

    assign hrdata    = fifo_read_data_out;
    assign haddr_wr  = {hwrite,haddr};
    assign hrw_en    = hready & hsel & htrans[1];
    assign hwrite_en = hwrite & hrw_en;
    assign hread_en  = !hwrite & hrw_en;

    always @(posedge hclk or negedge hrst_n) begin
        if (!hrst_n) begin
            haddr_sl <= {AW{1'b0}};
        end
        else if (hrw_en) begin
            haddr_sl <= haddr_wr;
        end
    end
    
    always @(posedge hclk or negedge hrst_n) begin
        if (!hrst_n) begin
            hwrite_en_reg <= 'b0;
        end
        else if (hready) begin
            hwrite_en_reg <= hwrite_en;
        end
    end

    wire hread_wdata;
    reg  reg_hread_wdata;
    assign hread_wdata  = hwrite_en_reg && hread_en;

    
    always @(posedge hclk or negedge hrst_n) begin
        if (!hrst_n) begin
            reg_hread_wdata <= 'b0;
        end
        else begin 
            reg_hread_wdata <= hread_wdata;       
        end
    end

    assign fifo_data_in = hwdata;
    assign fifo_addr_in = (hwrite_en_reg || reg_hread_wdata) ? haddr_sl : haddr_wr;

/************************hready state machine**********************/
    localparam IDLE = 3'b000;
    localparam WR   = 3'b001;
    localparam RD   = 3'b010;
    localparam RDOK = 3'b011;
    localparam FULL = 3'b100;
//    localparam PRRD = 3'b101;

    reg [2:0] current_state;
    reg [2:0] next_state;

    always @(posedge hclk or negedge hrst_n) begin
        if (!hrst_n) begin
            current_state <= IDLE;
        end
        else begin 
            current_state <= next_state;       
        end
    end

    always @(current_state or read_fifo_rd_en or fifo_full or hwrite_en or hread_en or reg_hread_wdata) begin
        case (current_state) 
            IDLE: begin
                if (hwrite_en)
                    next_state = WR;
                else if (hread_en && !fifo_full)
                    next_state = RD;                
                else if (fifo_full)
                    next_state = FULL;
                else 
                    next_state = IDLE;
                end
            WR: begin
                if (fifo_full )
                    next_state = FULL;
                else if (!hwrite_en)
                    next_state = IDLE;
                else if (reg_hread_wdata)
                    next_state = RD;
                else
                    next_state = WR;
            end
            FULL: begin
                if (!fifo_full && hwrite_en)
                    next_state = WR;
                //else if (!fifo_full && hread_en && htrans == 1'b11)
                  //  next_state = PRRD;
                else if (!fifo_full && hread_en)
                    next_state = RD;
                else if (!fifo_full && !hwrite_en)
                    next_state = IDLE;
                else
                    next_state = FULL;
            end
            RD: begin
                if (read_fifo_rd_en)
                    next_state = RDOK;
                //else if (htrans == 2'b11)
                //    next_state = PRRD;
                else 
                    next_state = RD;
            end
            RDOK: begin
                if (hread_en)
                    next_state = RD;
                else if (htrans == 2'b00)
                    next_state = IDLE;
                else
                    next_state = RDOK;
            end
/*            PRRD: begin
                if (htrans == 2'b10)
                    next_state = RD;
                else if (htrans == 2'b00)
                    next_state = IDLE;
                else if (fifo_full)
                    next_state = FULL;
                else
                    next_state = PRRD;
            end   */
            default: 
                    next_state = IDLE;
        endcase
    end

    assign hready       = !((current_state == RD) || (next_state == FULL));
    assign fifo_wr_en   = hready && hwrite_en_reg;
    assign fifo_addr_en = (hready && hwrite_en_reg) || hread_en || reg_hread_wdata;

/******************************SRAM CONTROL*****************************/
    wire                sram_wr_en; 
    wire                sram_rd_en;
    wire                sram_wdata; 
    wire    [AW-1:0]    sram_addr;
    reg                 read_fifo_wr_en_reg;
    reg                 sram_rw_en_reg;

    always @(posedge clk_sram or negedge rst_sram) begin
        if  (!rst_sram) begin
            sram_rw_en_reg <= 'b0;
        end
        else begin 
            sram_rw_en_reg <= addr_fifo_rd_en;
        end
    end

    always @(posedge clk_sram or negedge rst_sram) begin
        if (!rst_sram) begin
            read_fifo_wr_en_reg <=0;
        end
        else begin 
            read_fifo_wr_en_reg <= sram_rd_en; // Read data from SRAM
        end
    end

    assign sram_wr_en      = fifo_addr_out[AW] && sram_rw_en_reg;
    assign sram_rd_en      = !fifo_addr_out[AW] && sram_rw_en_reg;
    assign sram_wdata      = fifo_data_out;
    assign sram_addr       = fifo_addr_out[AW-1:0];
    assign read_fifo_wr_en =  read_fifo_wr_en_reg;

// SRAM INTERFACE
    sram_interface sram_if  (
        .clk_sram           ( clk_sram          ),
        .rst_sram           ( rst_sram          ),
        .we                 ( sram_wr_en        ),
        .re                 ( sram_rd_en        ),
        .addr               ( sram_addr         ),
        .wdata              ( fifo_data_out     ),
        .rdata              ( fifo_read_data_in )
    );

//write fifo
    async_fifo write_fifo(
        .clk_w      ( hclk          ),
        .rst_w      ( hrst_n        ),
        .wr_en      ( fifo_wr_en    ),
        .data_in    ( fifo_data_in  ),
        .clk_r      ( clk_sram      ),
        .rst_r      ( rst_sram      ),
        .data_out   ( fifo_data_out ),
        .rd_en      ( write_fifo_rd_en),
        .empty      (),
        .full       ()
    );

//addr fifo
    async_fifo # (16,AW+1,4) addr_fifo(
        .clk_w      ( hclk          ),
        .rst_w      ( hrst_n        ),
        .wr_en      ( fifo_addr_en  ),
        .data_in    ( fifo_addr_in  ),
        .clk_r      ( clk_sram      ),
        .rst_r      ( rst_sram      ),
        .data_out   ( fifo_addr_out ),
        .rd_en      ( addr_fifo_rd_en),
        .empty      ( addr_fifo_empty),
        .full       ( fifo_full      )
    );
    
 //read fifo
    async_fifo read_fifo(
        .clk_w      ( clk_sram          ),
        .rst_w      ( rst_sram          ),
        .wr_en      ( read_fifo_wr_en   ),
        .data_in    ( fifo_read_data_in ),
        .clk_r      ( hclk              ),
        .rst_r      ( hrst_n            ),
        .data_out   ( fifo_read_data_out),
        .rd_en      ( read_fifo_rd_en   ),
        .empty      ( read_fifo_empty   ),
        .full()
    );

endmodule

