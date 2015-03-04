% 2D Kalman filter for positional estimation
% TODO: not generalized to ND case

% Time step/frame
dt = 1/30; % sec

%% Rat states and dynamics
% Hidden State 
% Z(n) = FZ(n-1) + G*A where A ~ N(0,sig^2 x,y)

Z = [0 0 0 0]'; % [pos. x, vel. x, pos. y, vel. y]; m and m/s
G = [[ dt^2/2 0      ];
     [ dt     0      ];
     [ 0      dt^2/2 ];
     [ 0      dt     ]]; % random accel -> state vector transform
s_a = 0.1; % m/s^2, STD random accelerations


%% Filter states
% State transition model 
% Assuming Newtowian motion with instantaneous acceleration A, which is 
% indepdent and Guassian in both spatial dimensions 
F = [[ 1  dt 0  0  ];
     [ 0  1  0  0  ];
     [ 0  0  1  dt ];
     [ 0  0  0  1  ]];
 
% Covariance of process noise
Q = G*G'*s_a^2;

% Observation model (only have access to position)
H = [[ 1 0 0 0 ];
     [ 0 0 1 0 ]];
 
% Covariance of observation noise
s_x = 0.01; % 1 cm
s_y = s_x; % 1 cm
R = [[ s_x^2  0     ];
     [ 0      s_y^2 ]];

% Initial state estimate
Z_ = [0 0 0 0]';

% A posteriori error cov. matrix 
% We have no knowledge of this to begin with, so tell the filter
% that we assume this error is large, and it should weight new
% information more strongly than the model until it sees otherwise
ID = eye(numel(Z));
P = 10*ID;

close all
figure
hold on
i = 0;
while(1)
   
    % Simulate a rat movement (it follows the assumed model perfectly)
    A = [s_a*randn() s_a*randn()]';
    Z = F*Z + G*A; % true position (hidden)
    
    % Perform noisy measurement
    X = H*Z + [s_x*randn() s_y*randn()]';
    
    % Produce a-priori state estimate
    Z_ = F*Z_;
    
    % Predicted a-priori state estimate convariance
    P = F*P*F' + Q;
    
    % Innovation
    I = X - H*Z_;
    
    % Innovation covariance
    S = H*P*H' + R;
    
    % Optimal Kalman gain
    K = P*H'/S;
    
    % Produce a-posteriori state estimate
    Z_ = Z_ + K*I;
    
    % Predicted a-posteriori state estimate convariance
    P = (ID - K*H)*P;
    
    % Plot
    plot(Z(1), Z(3),'ko')
    plot(X(1), X(2),'b.')
    plot(Z_(1), Z_(3),'r.')
    title(['time: ' num2str(dt*i,2)]); 
    drawnow
    
    i = i+1;
end