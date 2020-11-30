module bitcoin_hash(
	input logic clk, reset_n, start,
	input logic [15:0] message_addr, output_addr,
	output logic done, mem_clk, mem_we,
	output logic [15:0] mem_addr,
	output logic [31:0] mem_write_data,
	input logic [31:0] mem_read_data
);

// The clock domain for all modules will be shared
	assign mem_clk = clk;

// SHA256 K constants
	parameter int k[0:63] = '{
		32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
		32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
		32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
		32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
		32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
		32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
		32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
		32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
	};

// LOCALPARAMETERS
	localparam IDLE = 4'h0, READMEM = 4'h1, WAITMEM = 4'h2, COMPUTE1 = 4'h3, UPDATEH = 4'h6, WRITEMSG = 4'h7, PREPWRT = 4'h8;
// REGISTERS
	reg [3:0] present_state, next_state;
// Internal Variables
	logic [31:0] A, B, C, D, E, F, G, H;
	
	logic [31:0] h [0:7];
	
	logic [31:0] hphase1 [0:7];
	 
	logic [31:0] w [0:15];
// Counters
	int memIndex, wordExpand, NONCE;
	logic [1:0] M;

// Main Logic
	always_ff @(posedge clk) begin
		case (present_state)
			IDLE: begin
				//$display("IDLING");
				mem_we <= 0;
				mem_addr <= message_addr;
				memIndex <= 0;
				M <= 0;
				wordExpand <= 1;
				
				h[0] <= 32'h6a09e667;
				h[1] <= 32'hbb67ae85;
				h[2] <= 32'h3c6ef372;
				h[3] <= 32'ha54ff53a;
				h[4] <= 32'h510e527f;
				h[5] <= 32'h9b05688c;
				h[6] <= 32'h1f83d9ab;
				h[7] <= 32'h5be0cd19;
				
				A <= 32'h6a09e667;
				B <= 32'hbb67ae85;
				C <= 32'h3c6ef372;
				D <= 32'ha54ff53a;
				E <= 32'h510e527f;
				F <= 32'h9b05688c;
				G <= 32'h1f83d9ab;
				H <= 32'h5be0cd19;
				
				
			end
			WAITMEM: begin
			//$display("WAITMEM: msg_addr: %h, mem_we: %d", mem_addr,mem_we);
				if( M == 0 )
					mem_addr <= message_addr + 16'd1;
				else
					mem_addr <= message_addr + 16'd17;
				
				done <= 0;
				wordExpand <= 1;
				
				//{ A, B, C, D, E, F, G, H } <= { h[0], h[1], h[2], h[3], h[4], h[5], h[6], h[7] };
				
			end
			READMEM: begin
				//$display("msg_addr: %h, M: %d, nonce: %d, memIndex: %d, mem_read_data: %h,", mem_addr, M,NONCE,memIndex, mem_read_data,);
				memIndex <= memIndex + 1; // Increment the index
				mem_addr <= mem_addr + 16'd1;
				
		if  (M==1 && memIndex == 3)  begin
					//$display("TRIGGERED");
						w[3] <= NONCE;
						w[4] <= 32'h80000000;
						w[5] <= 0;
						w[6] <= 0;
						w[7] <= 0;
						w[8] <= 0;
						w[9] <= 0;
						w[10] <= 0;
						w[11] <= 0;
						w[12] <= 0;
						w[13] <= 0;
						w[14] <= 0;
						w[15] <= 32'd640;
						
						h[0] <= hphase1[0];
						h[1] <= hphase1[1];
						h[2] <= hphase1[2];
						h[3] <= hphase1[3];
						h[4] <= hphase1[4];
						h[5] <= hphase1[5];
						h[6] <= hphase1[6];
						h[7] <= hphase1[7];
						
						A <= hphase1[0]; // Phase 2 should have the h's from phase 1
						B <= hphase1[1];
						C <= hphase1[2];
						D <= hphase1[3];
						E <= hphase1[4];
						F <= hphase1[5];
						G <= hphase1[6];
						H <= hphase1[7];
						
						//mem_addr <= mem_addr + 16'd12;
						end
		
		else if( M==2) begin
						
						w[0] <= h[0];
						w[1] <= h[1];
						w[2] <= h[2];
						w[3] <= h[3];
						w[4] <= h[4];
						w[5] <= h[5];
						w[6] <= h[6];
						w[7] <= h[7];
						w[8] <=  32'h80000000;
						w[9]  <= 0;
						w[10] <= 0;
						w[11] <= 0;
						w[12] <= 0;
						w[13] <= 0;
						w[14] <= 0;
						w[15] <= 32'd256;
						
						h[0] <= 32'h6a09e667;
						h[1] <= 32'hbb67ae85;
						h[2] <= 32'h3c6ef372;
						h[3] <= 32'ha54ff53a;
						h[4] <= 32'h510e527f;
						h[5] <= 32'h9b05688c;
						h[6] <= 32'h1f83d9ab;
						h[7] <= 32'h5be0cd19;
						
						A <= 32'h6a09e667;
						B <= 32'hbb67ae85;
						C <= 32'h3c6ef372;
						D <= 32'ha54ff53a;
						E <= 32'h510e527f;
						F <= 32'h9b05688c;
						G <= 32'h1f83d9ab;
						H <= 32'h5be0cd19;
						end
						
		
				
				else begin
				//$display("memIndex = %d",memIndex);
					w[memIndex] <= mem_read_data; end
			
			end
			
			
			
			COMPUTE1: begin
				//$display("Wordexpand round: %d", wordExpand);
				if ( wordExpand > 15 && wordExpand < 64) begin
					//$display("wordExpand[%2d]", wordExpand);
					w[wordExpand%16] <= wordexpansion( w[(wordExpand-16)%16], w[(wordExpand-15)%16], w[(wordExpand-7)%16], w[(wordExpand-2)%16]);
				end
				
				
				{ A, B, C, D, E, F, G, H } <= sha256_op( );
					
				wordExpand <= wordExpand + 1;
				
			end
			UPDATEH: begin
				//$display("updateH");
				h[0] <= h[0]+A;
				h[1] <= h[1]+B;
				h[2] <= h[2]+C;
				h[3] <= h[3]+D;
				h[4] <= h[4]+E;
				h[5] <= h[5]+F;
				h[6] <= h[6]+G;
				h[7] <= h[7]+H;
				
				if (M == 0) begin // Keep PHASE1 hash values in memory
				
				hphase1[0] <= h[0]+A;
				hphase1[1] <= h[1]+B;
				hphase1[2] <= h[2]+C;
				hphase1[3] <= h[3]+D;
				hphase1[4] <= h[4]+E;
				hphase1[5] <= h[5]+F;
				hphase1[6] <= h[6]+G;
				hphase1[7] <= h[7]+H;
				
				end

				M <= M + 1; // PREP MEM
				memIndex <= 0; // PREP MEM
				if(M < 2) // M =0: first to second phase, M = 1: second to third phase, M = 3: end of third phase
					mem_addr <= message_addr + 16'd16; // PREP MEM 
				else 
				begin
					mem_addr <= output_addr + NONCE;
					mem_we <= 1;
				end
			end
			PREPWRT: begin
				//mem_addr <= output_addr + NONCE;
				mem_write_data <= h[memIndex];
			end
			WRITEMSG: begin
				//$display("Write message: output address: %d, h0: %h", mem_addr, h[0]);
				//mem_addr <= output_addr + NONCE;
				//mem_write_data <= h[0];
				M <= 1;
				NONCE <= NONCE + 1;
				mem_addr <= message_addr + 16'd16;
				mem_we <= 0; // mem_we outside of if since we are only writing one word... could reduce a state here
				if( NONCE == 15) begin
					done <= 1;
				end
			end
		endcase
	end

// Next State Logic
	always_comb begin
		case(present_state)
			IDLE:
				if( start)
					next_state = WAITMEM;
				else
					next_state = IDLE;
			WAITMEM:
				next_state = READMEM;
			READMEM:
				if( memIndex == 15 || (memIndex == 3 && M ==1 ) || (M ==2) ) 
					next_state = COMPUTE1;
				else
					next_state = READMEM;
			COMPUTE1:
				if( wordExpand == 64 )
					next_state = UPDATEH;
				else
					next_state = COMPUTE1;
			UPDATEH:
				if( M < 2 ) // M= 0 for first to second phase, M = 1 for second to third phase. End of phase 2 should go straight to WAITMEM
					next_state = WAITMEM;
				else // M ==2, when it completed the third phase
					next_state = PREPWRT;
			PREPWRT:
				next_state = WRITEMSG;
			WRITEMSG:
				if( NONCE == 15 )
					next_state = IDLE;
				else
					next_state = WAITMEM;
		endcase
	end
	

// Right Rotation
function logic [31:0] rightrotate(input logic [31:0] x, input logic [7:0] r); begin
		rightrotate = (x >> r) | (x << (32-r));
	end
endfunction

// Word Expansion
function logic [31:0] wordexpansion( input logic [31:0] x16, x15, x7, x2 );
	begin
	logic [31:0] s0, s1;
		s0 = rightrotate(x15, 7) ^ rightrotate(x15, 18) ^ (x15 >> 3);
      s1 = rightrotate(x2, 17) ^ rightrotate(x2, 19) ^ (x2 >> 10);
      wordexpansion = x16 + s0 + x7 + s1;
	end
endfunction

// Word Expansion
function logic [255:0] sha256_op( );
	begin
	logic [31:0] s0, s1;
	logic [31:0] maj, ch, tee1, tee2;
		s0 = rightrotate(A,2) ^ rightrotate(A,13) ^ rightrotate(A,22);
		s1 = rightrotate(E,6) ^ rightrotate(E,11) ^ rightrotate(E,25);
      
		ch = ( E & F ) ^ ( (~E) & G );
		maj = ( A & B ) ^ ( A & C ) ^ ( B & C );
		
		tee2 = s0 + maj;
		tee1 = H + s1 + ch + k[wordExpand-1] + w[(wordExpand-1)%16];
		
		sha256_op = { tee1 + tee2, A, B, C, D + tee1, E, F ,G };
	end
endfunction


// State Register
	always_ff @ (posedge clk, negedge reset_n) begin
		if( !reset_n )
			present_state <= IDLE;
		else
			present_state <= next_state;
	end

endmodule: bitcoin_hash