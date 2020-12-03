module bitcoin_hash(
	input logic clk, reset_n, start,
	input logic [15:0] message_addr, output_addr,
	output logic done, mem_clk, mem_we,
	output logic [15:0] mem_addr,
	output logic [31:0] mem_write_data,
	input logic [31:0] mem_read_data
);

// Number of NONCES
	parameter int NUM_NONCES = 16;
	parameter int NN = 2;
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
	logic [31:0] A [0:NUM_NONCES], B[0:NUM_NONCES], C[0:NUM_NONCES], D[0:NUM_NONCES], E[0:NUM_NONCES], 
				 F[0:NUM_NONCES], G[0:NUM_NONCES], H[0:NUM_NONCES];
	
	logic [31:0] h [0:NUM_NONCES][0:7];
	
	logic [31:0] hphase1 [0:NUM_NONCES][0:7];
	 
	logic [31:0] w [0:NUM_NONCES][0:15];
	logic [31:0] s0[0:NUM_NONCES], s1 [0:NUM_NONCES];
	logic [31:0] maj[0:NUM_NONCES], ch[0:NUM_NONCES], tee1[0:NUM_NONCES], tee2[0:NUM_NONCES];
// Counters
	int memIndex, wordExpand;
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
				
				for( int n = 0; n <= NUM_NONCES; n++) begin
					h[n][0] <= 32'h6a09e667;
					h[n][1] <= 32'hbb67ae85;
					h[n][2] <= 32'h3c6ef372;
					h[n][3] <= 32'ha54ff53a;
					h[n][4] <= 32'h510e527f;
					h[n][5] <= 32'h9b05688c;
					h[n][6] <= 32'h1f83d9ab;
					h[n][7] <= 32'h5be0cd19;

					A[n] <= 32'h6a09e667;
					B[n] <= 32'hbb67ae85;
					C[n] <= 32'h3c6ef372;
					D[n] <= 32'ha54ff53a;
					E[n] <= 32'h510e527f;
					F[n] <= 32'h9b05688c;
					G[n] <= 32'h1f83d9ab;
					H[n] <= 32'h5be0cd19;
				
				end
			end
			WAITMEM: begin
			//$display("WAITMEM: msg_addr: %h, mem_we: %d", mem_addr,mem_we);
				if( M == 0 )
					mem_addr <= message_addr + 16'd1;
				else
					mem_addr <= message_addr + 16'd17;
				
				done <= 0;
				wordExpand <= 1;
				
			end
			READMEM: begin
				//$display("msg_addr: %h, M: %d, memIndex: %d, mem_read_data: %h,", mem_addr, M,memIndex, mem_read_data,);
				memIndex <= memIndex + 1; // Increment the index
				mem_addr <= mem_addr + 16'd1;
				
				for( int n = 0; n <= NUM_NONCES; n++ ) begin
					if  (M==1 && memIndex == 3)  begin
						w[n][3] <= n;
						w[n][4] <= 32'h80000000;
						w[n][5] <= 0;
						w[n][6] <= 0;
						w[n][7] <= 0;
						w[n][8] <= 0;
						w[n][9] <= 0;
						w[n][10] <= 0;
						w[n][11] <= 0;
						w[n][12] <= 0;
						w[n][13] <= 0;
						w[n][14] <= 0;
						w[n][15] <= 32'd640;
						
						h[n][0] <= hphase1[n][0];
						h[n][1] <= hphase1[n][1];
						h[n][2] <= hphase1[n][2];
						h[n][3] <= hphase1[n][3];
						h[n][4] <= hphase1[n][4];
						h[n][5] <= hphase1[n][5];
						h[n][6] <= hphase1[n][6];
						h[n][7] <= hphase1[n][7];
						
						A[n] <= hphase1[n][0];  // Phase 2 should have the h's from phase 1
						B[n] <= hphase1[n][1];
						C[n] <= hphase1[n][2];
						D[n] <= hphase1[n][3];
						E[n] <= hphase1[n][4];
						F[n] <= hphase1[n][5];
						G[n] <= hphase1[n][6];
						H[n] <= hphase1[n][7];
					end else if( M==2) begin		
						w[n][0] <= h[n][0];
						w[n][1] <= h[n][1];
						w[n][2] <= h[n][2];
						w[n][3] <= h[n][3];
						w[n][4] <= h[n][4];
						w[n][5] <= h[n][5];
						w[n][6] <= h[n][6];
						w[n][7] <= h[n][7];
						w[n][8] <=  32'h80000000;
						w[n][9]  <= 0;
						w[n][10] <= 0;
						w[n][11] <= 0;
						w[n][12] <= 0;
						w[n][13] <= 0;
						w[n][14] <= 0;
						w[n][15] <= 32'd256;
						
						h[n][0] <= 32'h6a09e667;
						h[n][1] <= 32'hbb67ae85;
						h[n][2] <= 32'h3c6ef372;
						h[n][3] <= 32'ha54ff53a;
						h[n][4] <= 32'h510e527f;
						h[n][5] <= 32'h9b05688c;
						h[n][6] <= 32'h1f83d9ab;
						h[n][7] <= 32'h5be0cd19;
						
						A[n] <= 32'h6a09e667;
						B[n] <= 32'hbb67ae85;
						C[n] <= 32'h3c6ef372;
						D[n] <= 32'ha54ff53a;
						E[n] <= 32'h510e527f;
						F[n] <= 32'h9b05688c;
						G[n] <= 32'h1f83d9ab;
						H[n] <= 32'h5be0cd19;
					end	else begin
						w[n][memIndex] <= mem_read_data;
					end
				end
			end
			
			
			
			COMPUTE1: begin
				for( int n = 0; n <= NUM_NONCES; n++) begin
					if ( wordExpand > 15 && wordExpand < 64) begin
						//$display("wordExpand[%2d]", wordExpand);
						w[n][wordExpand%16] <= wordexpansion( w[n][(wordExpand-16)%16], w[n][(wordExpand-15)%16], 
												w[n][(wordExpand-7)%16], w[n][(wordExpand-2)%16]);
					end
		
					s0[n] = rightrotate(A[n],2) ^ rightrotate(A[n],13) ^ rightrotate(A[n],22);
					s1[n] = rightrotate(E[n],6) ^ rightrotate(E[n],11) ^ rightrotate(E[n],25);

					ch[n] = ( E[n] & F[n] ) ^ ( (~E[n]) & G[n] );
					maj[n] = ( A[n] & B[n] ) ^ ( A[n] & C[n] ) ^ ( B[n] & C[n] );
					
					tee2[n] = s0[n] + maj[n];
					tee1[n] = H[n] + s1[n] + ch[n] + k[wordExpand-1] + w[n][(wordExpand-1)%16];
					
					{ A[n], B[n], C[n], D[n], E[n], F[n], G[n], H[n] } = { tee1[n] + tee2[n], A[n], B[n], C[n], D[n] + tee1[n], E[n], F[n] ,G[n] };
				end

				
				wordExpand <= wordExpand + 1;
				
			end
			UPDATEH: begin
				//$display("updateH");
				for( int n = 0; n <= NUM_NONCES; n++ ) begin
					h[n][0] <= h[n][0]+A[n]; // ADD N
					h[n][1] <= h[n][1]+B[n]; // ADD N
					h[n][2] <= h[n][2]+C[n]; // ADD N
					h[n][3] <= h[n][3]+D[n]; // ADD N
					h[n][4] <= h[n][4]+E[n]; // ADD N
					h[n][5] <= h[n][5]+F[n]; // ADD N
					h[n][6] <= h[n][6]+G[n]; // ADD N
					h[n][7] <= h[n][7]+H[n]; // ADD N
					
					if (M == 0) begin // Keep PHASE1 hash values in memory
						hphase1[n][0] <= h[n][0]+A[n]; //ADD N
						hphase1[n][1] <= h[n][1]+B[n]; //ADD N
						hphase1[n][2] <= h[n][2]+C[n]; //ADD N
						hphase1[n][3] <= h[n][3]+D[n]; //ADD N
						hphase1[n][4] <= h[n][4]+E[n]; //ADD N
						hphase1[n][5] <= h[n][5]+F[n]; //ADD N
						hphase1[n][6] <= h[n][6]+G[n]; //ADD N
						hphase1[n][7] <= h[n][7]+H[n]; //ADD N
					end
				end

				M <= M + 1; // PREP MEM
				memIndex <= 0; // PREP MEM
				if(M < 2) // M =0: first to second phase, M = 1: second to third phase, M = 3: end of third phase
					mem_addr <= message_addr + 16'd16; // PREP MEM 
				else 
				begin
					mem_addr <= output_addr;
					mem_we <= 1;
				end
			end
			PREPWRT: begin
				mem_write_data <= h[memIndex][0]; // ADD N
			end
			WRITEMSG: begin
				mem_addr <= output_addr + memIndex + 1;
				mem_write_data <= h[memIndex+1][0]; // ADD N
				memIndex <= memIndex + 1;
				if( memIndex == 15) begin
					done <= 1;
					mem_we <= 0;
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
				if( memIndex == 15 )
					next_state = IDLE;
				else
					next_state = WRITEMSG;
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


// State Register
	always_ff @ (posedge clk, negedge reset_n) begin
		if( !reset_n )
			present_state <= IDLE;
		else
			present_state <= next_state;
	end

endmodule: bitcoin_hash