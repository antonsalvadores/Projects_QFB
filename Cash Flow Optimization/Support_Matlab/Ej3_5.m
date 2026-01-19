function [X , fval , exitflag] = Ej3_5( f , A , b , Aeq , beq , lb , ub)

% A es una matriz que indica los coeficientes de las variables     
% de decisión en las restricciones de desigualdad (si no hay, A=0). %%
A = [];           

% b es un vector que indica los coeficientes del lado          
% derecho en las restricciones de desigualdad (si no hay, b=0). %%

b = [];

% Aeq es una matriz que indica los coeficientes de las    
% variables de decisión en las restricciones de igualdad.  %%

%      x1   x2    x3    x4    x5    y1    y2    y3    y4    y5    z1     z2    z3    z4    z5    v      %%
Aeq = [1    0     0     0     0     1     0     0     0     0     -1     0     0     0     0     0;
      -1.01 1     0     0     0     0     1     0     0     0     1.003 -1     0     0     0     0;
       0   -1.01  1     0     0    -1.02  0     1     0     0     0     1.003 -1     0     0     0;
       0    0     -1.01 1     0     0    -1.02  0     0     0     0      0    1.003 -1     0     0;
       0    0     0     -1.01 1     0     0    -1.02  0     0     0      0     0    1.003 -1     0;
       0    0     0     0     -1.01 0     0     0     0     0     0      0     0     0    1.003 -1];

% beq es un vector que indica los coeficientes       
% del lado derecho en las restricciones de igualdad.  %%

beq = [ 150 , 100 , -200 , 200 , -50 , -300 ];


% f es un vector que indica los coeficientes de las 
% variables de decisión en la función objetivo.      %%

f = zeros( 1 , length(Aeq(1,:))); f(end) = -1;       

% lb es un vector que indica los límites inferiores  de las variables de decisión,  
% normalmente ha de ser de la misma longitud que X, en caso contrario,              
% sea b la longitud de lb, menor que la longitud de X, estamos definiendo los       
% límites inferiores de las b primeras variables de decisión (como en nuestro caso). %%

lb = zeros( 1 , length(Aeq(1,:)) - 1 );

% ub es un vector que indica los límites superiores  de las variables de decisión,  
% normalmente ha de ser de la misma longitud que X, en caso contrario,              
% sea b la longitud de lb, menor que la longitud de X, estamos definiendo los       
% límites superiores de las b primeras variables de decisión (como en nuestro caso). %%

ub = [ 100 , 100 , 100 , 100 , 100 ];

% Resolvemos el problema, X nos devuelve los valores de las variables de decisión,  
% fval el valor de la función en el óptimo y exitflag si la solución es factible.    %%

[ X , fval , exitflag ] = linprog( f , A , b , Aeq , beq , lb , ub );
fval = -fval;   % Esto lo hacemos porque la función linprog resuelve por defecto
                % un problema de minimización, como este ejercicio es un
                % problema de maximización, multiplicamos por -1 los
                % coeficientes de la función objetivo y el valor de la
                % función en el óptimo y nos devuelve el resultado
                % correcto.

                    %% Mostrar los resultados en la ventana de comandos %%
disp('Valores de las variables de decisión (X):');
disp(X);

disp('Valor óptimo de la función objetivo (fval):');
disp(fval);

disp('Valor de exitflag:');
disp(exitflag);

                    %% si exitflag devuelve 1, bien, si devuelve 0, el algoritmo no ha convergido, %%
                    %% si devuelve -1, la solución es infactible                                   %%

% No se encuentra ninguna solución factible al reducir el plazo de pago del
% pagaré a dos meses, en vez de tres meses como en el ejemplo anterior. Por
% tanto, podemos concluir que es una restricción demasiado exigente el pago
% del pagaré a dos meses en vez de a tres meses, lo que imposibilita hacer
% frente al pago de la deuda en el último periodo.
