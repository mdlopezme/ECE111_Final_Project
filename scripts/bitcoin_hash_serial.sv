module bitcoin_hash(
	input logic clk, reset_n, start,
	input logic [15:0] message_addr, output_addr,
	output logic done, mem_clk, mem_we,
	output logic [15:0] mem_addr,
	output logic [31:0] mem_write_data,
	input logic [31:0] mem_read_data
);

// Number of NONCES + one, cuz dont feel like reindexing
parameter N = 0; //  # of machines
int NONCE; // # of nonces

// Copy from memory
logic [31:0] w [0:15][0:N];

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
localparam IDLE = 4'h0, PHASE0 = 4'h1, PREPHASE1 = 4'h2, PHASE1 = 4'h3, PREPHASE2 = 4'h4, PHASE2 = 4'h5, COMPUTE = 4'hc, PREPWRITE = 4'hd, WRITEMSG = 4'he, END = 4'hf;

// REGISTERS
reg [3:0] present_state, next_state;

// Counter
    int memIndex;
    logic [0:1] M;

// Internal Variables : Depends on N
    logic  [31:0] A[0:N], B[0:N], C[0:N], D[0:N], E[0:N], F[0:N], G[0:N], H[0:N],precomp[0:15];
    logic [31:0] h [0:7],Aphase0,Bphase0,Cphase0,Dphase0,Ephase0,Fphase0,Gphase0,Hphase0;
    logic [31:0] wphase2 [0:7][0:N];
// Internal Variables
    int computeCycle;

// Main Logic
    always_ff @ (posedge clk) begin
        case( present_state )
            IDLE: begin
                mem_we <= 0;
                mem_addr <= message_addr;
                memIndex <= 0;

                A[0] <= 32'h6a09e667;
                B[0] <= 32'hbb67ae85;
                C[0] <= 32'h3c6ef372;
                D[0] <= 32'ha54ff53a;
                E[0] <= 32'h510e527f;
                F[0] <= 32'h9b05688c;
                G[0] <= 32'h1f83d9ab;
                H[0] <= 32'h5be0cd19;

                h[0] <= 32'h6a09e667;
				h[1] <= 32'hbb67ae85;
				h[2] <= 32'h3c6ef372;
				h[3] <= 32'ha54ff53a;
				h[4] <= 32'h510e527f;
				h[5] <= 32'h9b05688c;
				h[6] <= 32'h1f83d9ab;
				h[7] <= 32'h5be0cd19;

                
            end
            PHASE0: begin // Moves all the data into a w variable
                M <= 0;
                mem_addr <= mem_addr + 16'd1;
                w[15][0] <= mem_read_data;
                done <= 0;

                for(int i = 0; i < 15; i++) w[i][0] <= w[i+1][0];
                memIndex <= memIndex + 1;
                computeCycle <= 15;
				if (memIndex == 1) 
					precomp[0] <= H[0] + k[memIndex-1];

				if (memIndex > 1 ) 
					precomp[0] <= G[0] + k[memIndex-1];
				

			 
			
                if( memIndex > 1 ) begin
                    { A[0], B[0], C[0], D[0], E[0], F[0], G[0], H[0] } <= sha256_op( 0, memIndex-2,precomp[0] );
                    //$display("A[%d]: %h", computeCycle-1, A[0]);
                end
                //$display("Read: %h, memIndex: %d", mem_read_data, memIndex);
            end
            PREPHASE1: begin 
		//	$display("Prephase 1 triggered, %d", NONCE);
                memIndex <= 0;
                mem_addr <= message_addr + 12'd16;
                M <= 1;

				
                for( int k = 0; k <= N; k++) begin 
				
				if (NONCE <1) begin

                    Aphase0 <= h[0] + A[0]; // Update ABCD...
                    Bphase0 <= h[1] + B[0];
                    Cphase0 <= h[2] + C[0];
                    Dphase0 <= h[3] + D[0];
                    Ephase0 <= h[4] + E[0];
                    Fphase0 <= h[5] + F[0];
                    Gphase0 <= h[6] + G[0];
                    Hphase0 <= h[7] + H[0];
					
					A[k] <= h[0] + A[0];
					B[k] <= h[1] + B[0];
					C[k] <= h[2] + C[0];
					D[k] <= h[3] + D[0];
					E[k] <= h[4] + E[0];
					F[k] <= h[5] + F[0];
					G[k] <= h[6] + G[0];
					H[k] <= h[7] + H[0];
					end
					
				else begin
					A[k] <= Aphase0;
					B[k] <= Bphase0;
					C[k] <= Cphase0;
					D[k] <= Dphase0;
					E[k] <= Ephase0;
					F[k] <= Fphase0;
					G[k] <= Gphase0;
					H[k] <= Hphase0;
					end
                
				end
				
				
				
				
				
			
            end
            PHASE1: begin
                M <= 1; // NOT NEEDED
                mem_addr <= mem_addr + 16'd1;
                memIndex <= memIndex + 1;
                computeCycle <= 15;  

                
                
                for( int t = 0; t <= N; t++) begin
				if (memIndex == 1) 
					precomp[t] <= H[t] + k[memIndex-1];

				if (memIndex > 1 ) 
					precomp[t] <= G[t] + k[memIndex-1];
				
                    if( memIndex <= 3 )
                        w[15][t] <= mem_read_data;
                    else if( memIndex == 4)
                        w[15][t] <= NONCE; // THe tth index
                    else if( memIndex == 5)
                        w[15][t] <= 32'h80000000;
                    else if( memIndex == 16)
                        w[15][t] <= 32'd640;
                    else
                        w[15][t] <= 0;

                    if( memIndex > 1 ) begin
                        { A[t], B[t], C[t], D[t], E[t], F[t], G[t], H[t] } <= sha256_op( t, memIndex-2,precomp[t] );
                        //$display("A[%d]: %h", computeCycle-1, A[0]);
                    end

                    for(int i = 0; i < 15; i++) w[i][t] <= w[i+1][t]; // Rotate w
                end

            end
            PREPHASE2: begin
                memIndex <= 0;
                mem_addr <= message_addr + 12'd16;
                M <= 2; 
                //$display("H[0]: %h", h[0][0] + A[0]);

                for( int k = 0; k <= N; k++) begin
                    h[0] <= 32'h6a09e667; // updateH
                    h[1] <= 32'hbb67ae85;
                    h[2] <= 32'h3c6ef372;
                    h[3] <= 32'ha54ff53a;
                    h[4] <= 32'h510e527f;
                    h[5] <= 32'h9b05688c;
                    h[6] <= 32'h1f83d9ab;
                    h[7] <= 32'h5be0cd19;

                    A[k] <= 32'h6a09e667; // Update ABCD...
                    B[k] <= 32'hbb67ae85;
                    C[k] <= 32'h3c6ef372;
                    D[k] <= 32'ha54ff53a;
                    E[k] <= 32'h510e527f;
                    F[k] <= 32'h9b05688c;
                    G[k] <= 32'h1f83d9ab;
                    H[k] <= 32'h5be0cd19;

                    // COULD BE ASSIGNED BY SHIFTING
                    wphase2[0][k] <= Aphase0 + A[k];
                    wphase2[1][k] <= Bphase0 + B[k];
                    wphase2[2][k] <= Cphase0 + C[k];
                    wphase2[3][k] <= Dphase0 + D[k];
                    wphase2[4][k] <= Ephase0 + E[k];
                    wphase2[5][k] <= Fphase0 + F[k];
                    wphase2[6][k] <= Gphase0 + G[k];
                    wphase2[7][k] <= Hphase0 + H[k];
                end
            end

            PHASE2: begin
                //$display("H[0]: %h", hphase2[memIndex][0]);
                memIndex <= memIndex + 1;
                computeCycle <= 15;

                for( int t = 0; t <= N; t++) begin
				if (memIndex == 1) 
					precomp[t] <= H[t] + k[memIndex-1];

				if (memIndex > 1 ) 
					precomp[t] <= G[t] + k[memIndex-1];

                    if( memIndex <= 8 )
                        w[15][t] <= wphase2[memIndex-1][t];
                    else if( memIndex == 9)
                        w[15][t] <= 32'h80000000; // THe kth index
                    else if( memIndex == 16)
                        w[15][t] <= 32'd256;
                    else
                        w[15][t] <= 0;

                    for(int i = 0; i < 15; i++) w[i][t] <= w[i+1][t];

                    if( memIndex > 1 ) begin
                        { A[t], B[t], C[t], D[t], E[t], F[t], G[t], H[t] } <= sha256_op( t, memIndex-2,precomp[t] );
                        //$display("A[%d]: %h", computeCycle-1, A[0]);
                    end
                end                   

            end

            COMPUTE: begin // NEEDS MAjor optimization

                
            
                
                for( int t = 0; t <= N; t++ ) begin
                    for(int i = 0; i < 15; i++)begin
                        w[i][t] <= w[i+1][t];
                    end 
					precomp[t] <= G[t]+ k[computeCycle+1];
                    
                    w[15][t] <= newWt( t ); // Calculate new W[t]
            
                    { A[t], B[t], C[t], D[t], E[t], F[t], G[t], H[t] } <= sha256_op( t, computeCycle,precomp[t] ); // Do a sha256 operation
                end

                computeCycle <= computeCycle + 1;
            end
            PREPWRITE: begin
               mem_addr <= output_addr + NONCE;
			   mem_we <= 1;
               memIndex <= 0;
			   mem_write_data <= h[0] + A[0];
            end
            WRITEMSG:begin
				NONCE <= NONCE + 1;
				mem_we <= 0; 
			end
            END: begin
                done <= 1;
            end
			
        endcase
		
    end
	

// Right Rotation
    function logic [31:0] rightrotate(input logic [31:0] x, input logic [7:0] r); begin
		rightrotate = (x >> r) | (x << (32-r));
	end
endfunction

// Word Expansion
function logic [31:0] newWt(input int n);
	begin
	logic [31:0] s0, s1;
		s0 = rightrotate(w[1][n], 7) ^ rightrotate(w[1][n], 18) ^ (w[1][n] >> 3);
        s1 = rightrotate(w[14][n], 17) ^ rightrotate(w[14][n], 19) ^ (w[14][n] >> 10);
        newWt = w[0][n] + s0 + w[9][n] + s1;
	end
endfunction

// Sha256 Operation
function logic [255:0] sha256_op( input int n, p,input logic [31:0] precomp);
	begin
	logic [31:0] s0, s1;
	logic [31:0] maj, ch, tee1, tee2;
		s0 = rightrotate(A[n],2) ^ rightrotate(A[n],13) ^ rightrotate(A[n],22);
		s1 = rightrotate(E[n],6) ^ rightrotate(E[n],11) ^ rightrotate(E[n],25);
      
		ch = ( E[n] & F[n] ) ^ ( (~E[n]) & G[n] );
		maj = ( A[n] & B[n] ) ^ ( A[n] & C[n] ) ^ ( B[n] & C[n] );
		
		tee2 = s0 + maj;
        tee1 = precomp +  s1 + ch  + w[15][n];


        
		
		sha256_op = { tee1 + tee2, A[n], B[n], C[n], D[n] + tee1, E[n], F[n] ,G[n] };
	end
endfunction


// Next State Logic
always_comb begin
    case(present_state)
        IDLE:
            if( start)
                next_state = PHASE0;
            else
                next_state = IDLE;
        PHASE0:
            if(memIndex == 16)
                next_state = COMPUTE;
            else
                next_state = PHASE0;
        PREPHASE1:
            next_state = PHASE1;
        PHASE1:
        if(memIndex == 16)
            next_state = COMPUTE;
        else
            next_state = PHASE1;
        PREPHASE2:
            next_state = PHASE2;
        PHASE2:
        if(memIndex == 16)
            next_state = COMPUTE;
        else
            next_state = PHASE2;
        COMPUTE:
            if( computeCycle == 63 && M == 0)
                next_state = PREPHASE1;
            else if( computeCycle == 63 && M == 1)
                next_state = PREPHASE2;
            else if( computeCycle == 63 && M == 2)
            next_state = PREPWRITE;
            else
                next_state = COMPUTE;
        PREPWRITE:
            next_state = WRITEMSG;
        WRITEMSG:
        if(NONCE == 15)
            next_state = END;
        else
            next_state = PREPHASE1;
        END:
            next_state = IDLE;
    endcase
end

// State Register
always_ff @ (posedge clk, negedge reset_n) begin
    if( !reset_n )
        present_state <= IDLE;
    else
        present_state <= next_state;
end

endmodule : bitcoin_hash