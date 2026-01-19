function [X , fval , exitflag] = Ej3_4(f , A , b , Aeq , beq , lb , ub)

% A es una matriz que indica los coeficientes de las variables      
% de decisión en las restricciones de desigualdad (si no hay, A=0). %%
A = [];           

% b es un vector que indica los coeficientes del lado               
% derecho en las restricciones de desigualdad (si no hay, b=0).     %%

b = [];

% Aeq es una matriz que indica los coeficientes de las              
% variables de decisión en las restricciones de igualdad.           %%

% Las filas de la matriz indican el número de restricciones         
% y las columnas las diferentes variables de decisión.              %%

%      x1    y1     y2     y3     y4     y5     y6     z1     z2     z3     z4      z5     z6     z7     k1    k2      k3      k4     k5     k6     k7      v       %%
Aeq = [1     1      0      0      0      0      0      1      0      0       0      0      0      0     -1      0       0      0      0      0      0      0;       %r1
      -0.01 -0.018  1      0      0      0      0     -1.025  1      0       0      0      0      0      1.005 -1       0      0      0      0      0      0;       %r2
      -0.01 -1.018 -0.018  1      0      0      0      0     -1.025  1       0      0      0      0      0      1.005  -1      0      0      0      0      0;       %r3
      -0.01  0     -1.018 -0.018  1      0      0      0      0     -1.025   1      0      0      0      0      0       1.005 -1      0      0      0      0;       %r4
      -0.01  0      0     -1.018 -0.018  1      0      0      0      0      -1.025  1      0      0      0      0       0      1.005 -1      0      0      0;       %r5
      -0.01  0      0      0     -1.018 -0.018  1      0      0      0       0     -1.025  1      0      0      0       0      0      1.005 -1      0      0;       %r6
      -0.01  0      0      0      0     -1.018 -0.018  0      0      0       0      0     -1.025  1      0      0       0      0      0      1.005 -1      0;       %r7
      -1.01  0      0      0      0      0     -1.018  0      0      0       0      0      0     -1.025  0      0       0      0      0      0      1.005 -1];      %r8

% beq es un vector que indica los coeficientes                      
% del lado derecho en las restricciones de igualdad.                %%

beq = [ 100 , 500 , 100 , -600 , -500 , 200 , 600 , -900 ];


% f es un vector que indica los coeficientes de las                 
% variables de decisión en la función objetivo.                      %%

f = zeros( 1 , length(Aeq(1,:))); f(end) = -1;       

% lb es un vector que indica los límites inferiores  de las variables de decisión,  
% normalmente ha de ser de la misma longitud que X, en caso contrario,              
% sea b la longitud de lb, menor que la longitud de X, estamos definiendo los       
% límites inferiores de las b primeras variables de decisión (como en nuestro caso). %%

lb = zeros( 1 , length(Aeq(1,:)) - 1 ) ;

% ub es un vector que indica los límites superiores  de las variables de decisión,  
% normalmente ha de ser de la misma longitud que X, en caso contrario,              
% sea b la longitud de lb, menor que la longitud de X, estamos definiendo los       
% límites superiores de las b primeras variables de decisión (como en nuestro caso). %%

ub = [];

% Resolvemos el problema, X nos devuelve los valores de las variables de decisión,  
% fval el valor de la función objetivo en el óptimo y exitflag si la solución es factible.    %%

[ X , fval , exitflag , lambda] = linprog( f , A , b , Aeq , beq , lb , ub );
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

disp('Análisis de sensibilidad:');
disp(lambda);

                    %% si exitflag devuelve 1, bien, si devuelve 0, el algoritmo no ha convergido, %%
                    %% si devuelve -1, la solución es infactible                                   %%


% Tenemos una solución óptima en donde se obtiene una riqueza de 471 346 €
% al principio del noveno trimestre. Para ello, deberíamos pedir un
% préstamo bianual de 429 409'9 € al inicio del primer trimestre, en el
% segundo trimestre un préstamo sexamestral de 178 212'1 € y en el tercer
% trimestre un préstamo trimestral de 107 451'9 €, lo quje generará
% distintos excesos de caja reinvertibles en los próximos trimestres
% (siguientes k_i) para así poder terminar el préstamo con la riqueza
% comentada previamente de 471 346 €.
