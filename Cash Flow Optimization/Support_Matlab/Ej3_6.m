function [X , fval , exitflag] = Ej3_6(f , A , b , Aeq , beq , lb , ub)

% A es una matriz que indica los coeficientes de las variables     
% de decisión en las restricciones de desigualdad (si no hay, A=0). %%
A = [];           

% b es un vector que indica los coeficientes del lado          
% derecho en las restricciones de desigualdad (si no hay, b=0). %%

b = [];

% Aeq es una matriz que indica los coeficientes de las    
% variables de decisión en las restricciones de igualdad.  %%
%      x1   x2    x3    x4    x5      %%
Aeq = [1    1     1     0     0   ; 
       2    1     0     1     0   ;
       3    4     0     0     1   ];

% beq es un vector que indica los coeficientes       
% del lado derecho en las restricciones de igualdad.  %%

beq = [ 100 , 150 , 360 ];



% f es un vector que indica los coeficientes de las 
% variables de decisión en la función objetivo.      %%

f = [ -4 ,  -3  ,  0  ,  0  ,  0  ];       

% lb es un vector que indica los límites inferiores  de las variables de decisión,  
% normalmente ha de ser de la misma longitud que X, en caso contrario,              
% sea b la longitud de lb, menor que la longitud de X, estamos definiendo los       
% límites inferiores de las b primeras variables de decisión (como en nuestro caso). %%

lb = [ 0 ,  0  ,  0  ,  0  ,  0  ];

% ub es un vector que indica los límites superiores  de las variables de decisión,  
% normalmente ha de ser de la misma longitud que X, en caso contrario,              
% sea b la longitud de lb, menor que la longitud de X, estamos definiendo los       
% límites superiores de las b primeras variables de decisión (como en nuestro caso). %%

ub = [];

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

% Se obtiene la misma solución que la encontrada durante el Capítulo 2. Las
% variables de decisión relevantes, x_1 y x_2, toman ambas el valor 50,
% mientras que la variable x_5, creada artificialmente para saturar la
% tercera restricción, es la única de las tres variables artificiales
% (x_3, x_4 y x_5) que toma un valor distinto de 0 (toma el valor 10),
% precisamente para poder saturar esta restricción. Como interpretación
% financiera, podemos añadir que para maximizar el beneficio de nuestra
% cartera, la debemos repartir a partes iguales entre bonos corporativos y
% bonos del Estado.
