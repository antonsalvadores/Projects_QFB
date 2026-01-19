function [X , fval , exitflag] = Ej3_12(f , A , b , Aeq , beq , lb , ub)

A=[];
b=[];
T = 9   ; % Número de Años
N = 16  ; % Número de Bonos
r = 0.02; % Ratio de reinversión (poner en decimales, torrijas...)

beq = [ 24e6   26e6   28e6   28e6   26e6 ...
        29e6   32e6   33e6   34e6 ] ; % Liabilities per year

P = [ 102.44   99.95   100.02   102.66    87.90 ...
       85.43   83.42   103.82   110.29   108.85 ...
      109.95   107.36  104.62    99.07   103.78 64.66]; % Price per bond

C = [ 5.625   4.75   4.25   5.25   0  ...
      0        0     5.75   6.875  6.5 ...
      6.625  6.125  5.625  4.75   5.5   0]; % Coupon per bond

M = [1 2 2 3 3 4 5 5 6 6 7 7 8 8 9 9]; % Maturity bond 
Aeq = zeros( T , N + T + 1); % Inicializamos matriz de restricciones

% La matriz tiene T filas correspondientes a los T años, 
% las primeras N columnas se corresponden a las variables de decisión x_i
% para los i=1,...,N bonos, que indican cuántos bonos i hemos de adquirir
% en el momento de construir la cartera (será el Año 0) y las siguientes 
% T + 1 columnas se corresponden a los posibles excedentes z_t para
% t=0,...,1 del rendimiento de los bonos que son reinvertibles a un ratio r
% (en este ejercicio, 2%). Hay T + 1 excedentes porque consideraremos 
% un posible "excedente" en el Año 0, es decir, en el momento de construir
% la cartera, la posibilidad de invertir ese dinero al ratio r en lugar
% de invertirlo en algún bono.

for t = 1:T
    for i = 1:N
        if M(i) == t
            Aeq(t,i) = C(i) + 100; % En el año de caducidad del bono nos pagan el cupón más el precio nominal del bono (100$).
        elseif M(i) > t
            Aeq(t,i) = C(i);       % El cupón del bono dentro de un año normal.
        else
            Aeq(t,i) = 0;          % Si ya caducó el bono evidentemente no paga intereses.
        end
    end
    Aeq(t,N+t) = 1 + r ; Aeq(t,N+t+1) = -1; % El rendimiento del excedente del año anterior y la inversión del excedente del año actual.
end
            



% f es un vector que indica los coeficientes de las 
% variables de decisión en la función objetivo.      %%

f = [P.*ones(1,N) , 1 , zeros( 1 , T)];  % Minimizamos el precio del bono por la cantidad que tenemos
                                         % y el "excedente" del primer año
                                         % que invertimos al ratio r.

% lb es un vector que indica los límites inferiores  de las variables de decisión,  
% normalmente ha de ser de la misma longitud que X, en caso contrario,              
% sea b la longitud de lb, menor que la longitud de X, estamos definiendo los       
% límites inferiores de las b primeras variables de decisión (como en nuestro caso). %%

lb = zeros( 1 , length(Aeq(1,:))) ;

% ub es un vector que indica los límites superiores  de las variables de decisión,  
% normalmente ha de ser de la misma longitud que X, en caso contrario,              
% sea b la longitud de lb, menor que la longitud de X, estamos definiendo los       
% límites superiores de las b primeras variables de decisión (como en nuestro caso). %%

ub = [];

                    %% Resolvemos el problema, X nos devuelve los valores de las variables de decisión,  %%
                    %% fval el valor de la función en el óptimo y exitflag si la solución es factible    %%

[ X , fval , exitflag ] = linprog( f , A , b , Aeq , beq , lb , ub );

                    %% Mostrar los resultados en la ventana de comandos %%
disp('Valores de las variables de decisión (X):');
disp(X);

disp('Valor óptimo de la función objetivo (fval):');
disp(fval);

disp('Valor de exitflag:');
disp(exitflag);

disp(Aeq);

% La combinación óptima para minimizar el pago inicial para formar la
% cartera y hacer frente a todas las obligaciones durante los siguientes 9
% años sería adquirir 145 670 bonos x_1, 173 860 bonos x_2, 202 120 bonos x_5
% y x_6, 182 120 bonos x_8, 222 590 bonos x_9, 267 900 bonos x_12, 295 640
% bonos x_14 y 322 270 bonos x_16. Además, cabe mencionar que no hay ningún
% excedente para ningún año, por lo que podemos concluir que la opción de
% reinversión al 2% no es utilizada, en concreto, es peor opción de
% inversión que los propios bonos en sí. Gracias a la elección de esta
% cartera de bonos, estamos gastando en el instante inicial 189 600 000 $
% para cumplir con todas las obligaciones de pensiones durante el periodo
% de los siguientes 9 años, que sumarían en total 260 000 000 $, así que
% estamos ahorrando cerca de 70 000 000 $ gracias a esta inversión en
% bonos.
