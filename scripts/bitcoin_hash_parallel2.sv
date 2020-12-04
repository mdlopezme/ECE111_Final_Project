module  computer #(parameter N = 0) (
    input logic [31:0] hh [0:7],
    input logic reset_n,
	input logic clk,
    input logic start,
    output logic done,
    output logic [31:0] answer [0:N],
    input [31:0] pass[0:2]
);

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
	localparam IDLE = 4'h0, SETW = 4'h1, COMPUTE1 = 4'h2, UPDATEH = 4'h3, SET = 4'h9;
// REGISTERS
	reg [3:0] present_state, next_state;
// Internal Variables
logic [31:0] w [0:N][0:15];
logic  [31:0] A[0:N], B[0:N], C[0:N], D[0:N], E[0:N], F[0:N], G[0:N], H[0:N];
logic [31:0] h [0:N][0:7];
int wordExpand;
logic M;

// Main Generator Logic
genvar i;
generate
    for(i=0; i <= N; i++) begin : hasher
        always_ff @(posedge clk) begin
            case(present_state)
                IDLE: begin
                    //$display("N=%d", N);
                    h[i][0] <= hh[0];
                    h[i][1] <= hh[1];
                    h[i][2] <= hh[2];
                    h[i][3] <= hh[3];
                    h[i][4] <= hh[4];
                    h[i][5] <= hh[5];
                    h[i][6] <= hh[6];
                    h[i][7] <= hh[7];

                    A[i] <= hh[0];
                    B[i] <= hh[1];
                    C[i] <= hh[2];
                    D[i] <= hh[3];
                    E[i] <= hh[4];
                    F[i] <= hh[5];
                    G[i] <= hh[6];
                    H[i] <= hh[7];
                end
                SETW: begin
                    //$display("SETW: A[0] = %h", A[0]);
                    if(M) begin
                        h[i][0] <= 32'h6a09e667;
                        h[i][1] <= 32'hbb67ae85;
                        h[i][2] <= 32'h3c6ef372;
                        h[i][3] <= 32'ha54ff53a;
                        h[i][4] <= 32'h510e527f;
                        h[i][5] <= 32'h9b05688c;
                        h[i][6] <= 32'h1f83d9ab;
                        h[i][7] <= 32'h5be0cd19;
                        
                        w[i][0] <= h[0][0] + A[0];
                        w[i][1] <= h[0][1] + B[0];
                        w[i][2] <= h[0][2] + C[0];
                        w[i][3] <= h[0][3] + D[0];
                        w[i][4] <= h[0][4] + E[0];
                        w[i][5] <= h[0][5] + F[0];
                        w[i][6] <= h[0][6] + G[0];
                        w[i][7] <= h[0][7] + H[0];
                        w[i][8] <= 32'h80000000;
                        w[i][9] <= 0;
                        w[i][10] <=0;
                        w[i][11] <=0;
                        w[i][12] <=0;
                        w[i][13] <=0;
                        w[i][14] <=0;
                        w[i][15] <= 32'd256;

                        A[i] <= 32'h6a09e667;
						B[i] <= 32'hbb67ae85;
						C[i] <= 32'h3c6ef372;
						D[i] <= 32'ha54ff53a;
						E[i] <= 32'h510e527f;
						F[i] <= 32'h9b05688c;
						G[i] <= 32'h1f83d9ab;
						H[i] <= 32'h5be0cd19;
                    end else begin
                        w[i][0] <= pass[0];
                        w[i][1] <= pass[1];
                        w[i][2] <= pass[2];
                        w[i][3] <= i; // Nonce Counter
                        w[i][4] <= 32'h80000000;
                        w[i][5] <= 0;
                        w[i][6] <= 0;
                        w[i][7] <= 0;
                        w[i][8] <= 0;
                        w[i][9] <= 0;
                        w[i][10] <= 0;
                        w[i][11] <= 0;
                        w[i][12] <= 0;
                        w[i][13] <= 0;
                        w[i][14] <= 0;
                        w[i][15] <= 32'd640;
                    end
                end
                COMPUTE1: begin
                    if ( wordExpand > 15 && wordExpand < 64) begin
						w[i][wordExpand%16] <= wordexpansion( w[i][(wordExpand-16)%16], w[i][(wordExpand-15)%16], w[i][(wordExpand-7)%16], w[i][(wordExpand-2)%16]);
					end
                    { A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i] } <= sha256_op( i );
                    
                end
                SET: begin
                    answer[i] <= h[i][0]+A[i];
                end
            endcase
        end
    end
endgenerate

// Main Logic
    always_ff @(posedge clk) begin
        case(present_state)
            IDLE: begin
                M <= 0;
            end
            SETW: begin
                wordExpand <= 1;
                if(wordExpand > 63) begin
                    M <= 1;
                end
            end
            SET: begin
                done <= 1;
            end
            COMPUTE1: begin
                wordExpand <= wordExpand + 1;
            end
        endcase

        
    end

// Next State Logic
	always_comb begin
		case(present_state)
            IDLE:
                if(start)
                    next_state = SETW;
                else
                    next_state = IDLE;
            SETW:
                next_state = COMPUTE1;
            COMPUTE1:
                if( wordExpand == 64 && ~M)
                    next_state = SETW;
                else if( wordExpand == 64 && M)
                    next_state = SET;
				else
					next_state = COMPUTE1;
            SET:
                next_state = IDLE;
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
// Sha256 operation
function logic [255:0] sha256_op( input int t );
	begin
	logic [31:0] s0, s1;
	logic [31:0] maj, ch, tee1, tee2;
		s0 = rightrotate(A[t],2) ^ rightrotate(A[t],13) ^ rightrotate(A[t],22);
		s1 = rightrotate(E[t],6) ^ rightrotate(E[t],11) ^ rightrotate(E[t],25);
      
		ch = ( E[t] & F[t] ) ^ ( (~E[t]) & G[t] );
		maj = ( A[t] & B[t] ) ^ ( A[t] & C[t] ) ^ ( B[t] & C[t] );
		
		tee2 = s0 + maj;
        tee1 = H[t] + s1 + ch + k[wordExpand-1] + w[t][(wordExpand-1)%16];
        $display("A[%d]=%h | %h", wordExpand-1, tee1+tee2, w[t][(wordExpand-1)%16]);
		
		sha256_op = { tee1 + tee2, A[t], B[t], C[t], D[t] + tee1, E[t], F[t] ,G[t] };
	end
endfunction
// State Register
	always_ff @ (posedge clk, negedge reset_n) begin
		if( !reset_n )
			present_state <= IDLE;
		else
			present_state <= next_state;
	end
endmodule : computer