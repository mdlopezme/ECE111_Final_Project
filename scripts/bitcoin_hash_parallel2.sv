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
    //localparam int t = 5;
// REGISTERS
	reg [3:0] present_state, next_state;
// Internal Variables
logic [31:0] w [0:15][0:N];
logic  [31:0] A[0:N], B[0:N], C[0:N], D[0:N], E[0:N], F[0:N], G[0:N], H[0:N];
logic [31:0] h [0:7][0:N];
int wordExpand;
logic M;

// Main Logic
    always_ff @ (posedge clk) begin
        case(present_state)
            IDLE: begin
                M <= 0;
            end
            SETW: begin
                wordExpand <= 1;
            end
            COMPUTE1: begin
                wordExpand <= wordExpand + 1;
                
                if(wordExpand > 63)
                    M <= 1;
            end
            SET: begin
                //$display("M[2]: h[0]: %h",  h[0][t]+A[t]);
                //answer[0] <= h[0][0]+A[t];
                done <= 1;
            end
        endcase
    end

// Main Generator Logic
genvar t;
generate
    for( t = 0; t <= N; t++) begin : hasher
        always_ff @(posedge clk) begin
            case(present_state)
                IDLE: begin
                    //$display("N=%d", N);
                    h[0][t] <= hh[0];
                    h[1][t] <= hh[1];
                    h[2][t] <= hh[2];
                    h[3][t] <= hh[3];
                    h[4][t] <= hh[4];
                    h[5][t] <= hh[5];
                    h[6][t] <= hh[6];
                    h[7][t] <= hh[7];

                    A[t] <= hh[0];
                    B[t] <= hh[1];
                    C[t] <= hh[2];
                    D[t] <= hh[3];
                    E[t] <= hh[4];
                    F[t] <= hh[5];
                    G[t] <= hh[6];
                    H[t] <= hh[7];
                end
                SETW: begin
                    //$display("SETW: A[0] = %h", A[0]);
                    if(~M)  begin
                        $display("M[0]: h[0]: %h",  hh[0]);
                        w[0][t] <= pass[0];
                        w[1][t] <= pass[1];
                        w[2][t] <= pass[2];
                        w[3][t] <= t; // Nonce Counter
                        w[4][t] <= 32'h80000000;
                        w[5][t] <= 0;
                        w[6][t] <= 0;
                        w[7][t] <= 0;
                        w[8][t] <= 0;
                        w[9][t] <= 0;
                        w[10][t] <= 0;
                        w[11][t] <= 0;
                        w[12][t] <= 0;
                        w[13][t] <= 0;
                        w[14][t] <= 0;
                        w[15][t] <= 32'd640;
                    end else begin
                        $display("M[1]: h[0]: %h",  h[0][t]+A[t]);
                        w[0][t] <= h[0][t] + A[t];
                        w[1][t] <= h[1][t] + B[t];
                        w[2][t] <= h[2][t] + C[t];
                        w[3][t] <= h[3][t] + D[t];
                        w[4][t] <= h[4][t] + E[t];
                        w[5][t] <= h[5][t] + F[t];
                        w[6][t] <= h[6][t] + G[t];
                        w[7][t] <= h[7][t] + H[t];
                        w[8][t] <= 32'h80000000;
                        w[9][t] <= 0;
                        w[10][t] <= 0;
                        w[11][t] <= 0;
                        w[12][t] <= 0;
                        w[13][t] <= 0;
                        w[14][t] <= 0;
                        w[15][t] <= 32'd256;

                        h[0][t] <= 32'h6a09e667;
                        h[1][t] <= 32'hbb67ae85;
                        h[2][t] <= 32'h3c6ef372;
                        h[3][t] <= 32'ha54ff53a;
                        h[4][t] <= 32'h510e527f;
                        h[5][t] <= 32'h9b05688c;
                        h[6][t] <= 32'h1f83d9ab;
                        h[7][t] <= 32'h5be0cd19;

                        A[t] <= 32'h6a09e667;
                        B[t] <= 32'hbb67ae85;
                        C[t] <= 32'h3c6ef372;
                        D[t] <= 32'ha54ff53a;
                        E[t] <= 32'h510e527f;
                        F[t] <= 32'h9b05688c;
                        G[t] <= 32'h1f83d9ab;
                        H[t] <= 32'h5be0cd19;
                    end
                end
                COMPUTE1: begin
                    if ( wordExpand > 15 && wordExpand < 64) begin
                        //$display("wordExpand[%2d]", wordExpand);
                        w[wordExpand%16][t] <= wordexpansion( w[(wordExpand-16)%16][t], w[(wordExpand-15)%16][t], w[(wordExpand-7)%16][t], w[(wordExpand-2)%16][t]);
                    end
                    
                    { A[t], B[t], C[t], D[t], E[t], F[t], G[t], H[t] } <= sha256_op( t );
                    
                end
                SET: begin
                    //$display("M[2]: h[0]: %h",  h[0][t]+A[t]);
                    answer[t] <= h[0][t]+A[t];
                end
            endcase
        end
    end
endgenerate

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
                if( wordExpand == 64 & ~M )
                    next_state = SETW;
                else if( wordExpand == 64 & M )
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

// Sha256 Operation
function logic [255:0] sha256_op( input int n);
	begin
	logic [31:0] s0, s1;
	logic [31:0] maj, ch, tee1, tee2;
		s0 = rightrotate(A[n],2) ^ rightrotate(A[n],13) ^ rightrotate(A[n],22);
		s1 = rightrotate(E[n],6) ^ rightrotate(E[n],11) ^ rightrotate(E[n],25);
      
		ch = ( E[n] & F[n] ) ^ ( (~E[n]) & G[n] );
		maj = ( A[n] & B[n] ) ^ ( A[n] & C[n] ) ^ ( B[n] & C[n] );
		
		tee2 = s0 + maj;
		tee1 = H[n] + s1 + ch + k[wordExpand-1] + w[(wordExpand-1)%16][n];
		
		sha256_op = { tee1 + tee2, A[n], B[n], C[n], D[n] + tee1, E[n], F[n] ,G[n] };
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