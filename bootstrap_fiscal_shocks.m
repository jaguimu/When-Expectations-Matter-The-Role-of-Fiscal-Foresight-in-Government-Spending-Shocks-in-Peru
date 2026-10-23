%% Anticipated shocks in fiscal policy in Peru

%% Load data
randn('seed',1234);
clear all;
clc;

y=xlsread('espe1.xlsx');
y_evar=y;

% Base VAR variables (without forecast)
y_base=y(:,[1,3,4,5]);

% Government forecast (extra variable for EVAR)
gob_for =  y(:,2);

% Estimate base VAR(1)
Mdl = varm(4,1);
[esti_1,~,~,E] = estimate(Mdl,y_base);
impulse_model1=irf(esti_1);

% Coefficients: [constant, AR matrix]
PHI_hat_base=esti_1.AR;
constant=esti_1.Constant;
PHI_hat_base=cell2mat(PHI_hat_base);
bigPI_hat_base= [constant PHI_hat_base];

%% Variable order
%ti.dif
%gov.dif
%gdp.dif
%cons.dif

%% VAR MODEL

% Build regressors and residuals
T = size(y_base,1);
X_base = [ ones(T-1,1) y_base(1:T-1,:) ];
Y_base = y_base(2:T,:);
T=length(Y_base);
Y_hat_base     = X_base*bigPI_hat_base';
e_hat_base     = Y_base - Y_hat_base;
eps=e_hat_base;
length(eps);
x1=y_base;
beta=bigPI_hat_base';


%% EVAR MODEL

% Estimate VAR(1) with forecast variable
Md2 = varm(5,1);
[esti_2,~,~,E] = estimate(Md2,y_evar);
impulse_model2=irf(esti_2);
PHI_hat_evar=esti_2.AR;
PHI_hat_evar=cell2mat(PHI_hat_evar);
constant_evar=esti_2.Constant;
bigPI_hat_evar= [constant_evar PHI_hat_evar];

% Residuals
T = size(y_evar,1);
X_evar = [ ones(T-1,1) y_evar(1:T-1,:) ];
Y_evar = y_evar(2:T,:);
T=length(Y_evar);
Y_hat_evar     = X_evar*bigPI_hat_evar';
e_hat_evar     = Y_evar - Y_hat_evar;
eps_evar=e_hat_evar;
x1_evar=y_evar;
beta_evar=bigPI_hat_evar';


%% BOOTSTRAP

N=1000;   % Number of draws
P=1;      % Lags
boot_gdp_fisco_evar=zeros(20,N); 
boot_cons_fisco_evar=zeros(20,N);
boot_gdp_fisco_base=zeros(20,N); 
boot_cons_fisco_base=zeros(20,N);
DD_gdp = zeros(N,1);
DD_cons = zeros(N,1);

for n=1:N
    
    P=1;
    draw = unidrnd(T,T,1);  % Random draws with replacement
    epshat = eps(draw,:);   % Resampled residuals
    
    % Simulate artificial data
    x1new = zeros(T+P,4);
    x1new(1,:) = x1(1,:);   % Initial value
    
    for t=P+1:T+P
        x1new(t,:) = epshat(t-P,:);   % Add shock
        for p=1:P
            x1new(t,:) = x1new(t,:)+x1new(t-p,:)*beta(2:5,:);   % Add lag effect
        end
    end    
%ti.dif
%pred.dif
%gov.dif
%gdp.dif
%cons.dif

    % IRF with base VAR
    Model_base = varm(4,1);
    x1new_base = x1new;
    Model_base = estimate(Model_base,x1new_base);
    impulse_base=irf(Model_base); 

    irf_gdp_fisco=impulse_base(:,2,3);
    irf_cons_fisco=impulse_base(:,2,4); 


    % IRF with EVAR
    Model_evar = varm(5,1);
    x1new_evar = [x1new(:,1) gob_for x1new(:,2) x1new(:,3) x1new(:,4)]         ;
    Model_evar = estimate(Model_evar,x1new_evar);   
    impulse_evar=irf(Model_evar); 

    irf_gdp_evar=impulse_evar(:,3,4);
    irf_cons_evar=impulse_evar(:,3,5);
    
    % D statistic (max gap between IRFs)
    D_gdp = transpose(abs(max(irf_gdp_fisco-irf_gdp_evar)));
    DD_gdp(n,1)= D_gdp;
    
    D_cons = transpose(abs(max(irf_cons_fisco-irf_cons_evar)));
    DD_cons(n,1)= D_cons;
   
end

%% IRFs from original models

impulse_model1;
impulse_model2;

irf_gdp_fisco=impulse_model1(:,2,3);
irf_cons_fisco=impulse_model1(:,2,4);   
irf_gdp_evar=impulse_model2(:,3,4);
irf_cons_evar=impulse_model2(:,3,5); 

%% D statistic distribution
figure(5);
gg=histogram(DD_gdp); 

figure(6);
gg=histogram(DD_cons); 

%% Observed D statistic

D_gdp_obs=irf_gdp_fisco-irf_gdp_evar;
D_gdp_obs=max(abs(D_gdp_obs));

D_cons_obs=irf_cons_fisco-irf_cons_evar;
D_cons_obs=max(abs(D_cons_obs));

%% P-values

p_value_gdp = length(DD_gdp(D_gdp_obs<DD_gdp,1))/N; 
p_value_gdp

p_value_cons = length(DD_cons(D_cons_obs<DD_cons,1))/N; 
p_value_cons

%% IRF plots

% 90% bands
bandas11 = quantile( boot_gdp_fisco_evar , [0.1 0.9], 2);
bandas12 = quantile( boot_gdp_fisco_base , [0.1 0.9], 2);

bandas21 = quantile( boot_cons_fisco_evar , [0.1 0.9], 2);
bandas22 = quantile( boot_cons_fisco_base , [0.1 0.9], 2);

% Gov. spending shock on GDP
figure(7)
plot(irf_gdp_evar, 'o-r')
title('Shock del Gasto pùblico sobre el PBI (IC al 90%)')
hold on
plot(irf_gdp_fisco, 'o-k')
plot(bandas11, ':r')
plot(bandas12, ':k')
legend('EVAR', 'VAR', 'ic(90%)', 'ic(90%)' , 'ic(90%)', 'ic(90%)')
hold off

% Gov. spending shock on private consumption
figure(8)
plot(irf_cons_evar, 'o-r')
title('Shock del Gasto pùblico sobre el Consumo Privado (IC al 90%)')
hold on
plot(irf_cons_fisco, 'o-k')
hold off
hold on
plot(bandas11, ':r')
plot(bandas12, ':k')
legend('EVAR', 'VAR', 'ic(90%)', 'ic(90%)' , 'ic(90%)', 'ic(90%)')
hold off

% Simulated vs observed D (GDP)
figure(9)
histogram(DD_gdp, 'FaceColor','black','EdgeColor','black','FaceAlpha',0.2,'EdgeAlpha',1)
xline(D_gdp_obs, 'o-r', 'linewidth',2)
legend('$D^y$ simulado', '$D^y$ observado','Interpreter','latex' )
shg;

% Simulated vs observed D (consumption)
figure(10)
histogram(DD_cons, 'FaceColor','black','EdgeColor','black','FaceAlpha',0.2,'EdgeAlpha',1)
xline(D_cons_obs, 'o-r', 'linewidth',2)
legend('$D^c$ simulado', '$D^c$ observado','Interpreter','latex' )
shg;
