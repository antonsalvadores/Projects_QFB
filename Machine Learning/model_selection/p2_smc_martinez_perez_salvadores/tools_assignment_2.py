# -*- coding: utf-8 -*-
"""
Machine learning: Exercise sheet 1

Auxiliary function for plotting the results of the classsifiers

This is a temporary script file.
"""

import pandas as pd
import numpy as np
import math
import matplotlib.pyplot as plt
import seaborn as sns
sns.set(style="whitegrid", context="notebook")

from sklearn.model_selection import train_test_split, GridSearchCV, cross_val_score, StratifiedKFold
from sklearn.preprocessing import OneHotEncoder, RobustScaler, StandardScaler, FunctionTransformer
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline
from sklearn.impute import SimpleImputer

from sklearn.neighbors import KNeighborsClassifier
from sklearn.tree import DecisionTreeClassifier, plot_tree
from sklearn.neural_network import MLPClassifier
from sklearn.model_selection import cross_val_predict

from sklearn.metrics import (
    make_scorer,
    fbeta_score,
    recall_score,
    precision_score,
    accuracy_score,
    classification_report,
    confusion_matrix,
    ConfusionMatrixDisplay,
    RocCurveDisplay
    )


def plot_classifiers(classifiers, datasets):
    
    _ = plt.figure(figsize=(4*6, 4*4))
    h = .02  # step size in the mesh
    i = 1
    # iterate over datasets
    for ds_cnt, ds in enumerate(datasets):
        # preprocess dataset, split into training and test part
        X, y = ds
        X = StandardScaler().fit_transform(X)
        X_train, X_test, y_train, y_test = train_test_split(
            X, 
            y,
            test_size=.4,
            random_state=42
        )
        x_min, x_max = X[:, 0].min() - .5, X[:, 0].max() + .5
        y_min, y_max = X[:, 1].min() - .5, X[:, 1].max() + .5
        xx, yy = np.meshgrid(np.arange(x_min, x_max, h),
                             np.arange(y_min, y_max, h))
        
        # just plot the dataset first
        cm = plt.cm.RdBu
        cm_bright = ListedColormap(['#FF0000', '#0000FF'])
        ax = plt.subplot(len(datasets), len(classifiers) + 1, i)
        if ds_cnt == 0:
            ax.set_title("Input data", fontsize=24)
        # Plot the training points
        ax.scatter(X_train[:, 0], X_train[:, 1], c=y_train, cmap=cm_bright,
                   edgecolors='k')
        # and testing points
        ax.scatter(X_test[:, 0], X_test[:, 1], c=y_test, cmap=cm_bright, alpha=0.6,
                   edgecolors='k')
        ax.set_xlim(xx.min(), xx.max())
        ax.set_ylim(yy.min(), yy.max())
        ax.set_xticks(())
        ax.set_yticks(())
        i += 1
        
        # iterate over classifiers
        for name, clf in classifiers:
            ax = plt.subplot(len(datasets), len(classifiers) + 1, i)
            clf.fit(X_train, y_train)
            score = clf.score(X_test, y_test)
            
            # Plot the decision boundary. For that, we will assign a color to each
            # point in the mesh [x_min, x_max]x[y_min, y_max].
            if hasattr(clf, "decision_function"):
                Z = clf.decision_function(np.c_[xx.ravel(), yy.ravel()])
            else:
                Z = clf.predict_proba(np.c_[xx.ravel(), yy.ravel()])[:, 1]
            
            # Put the result into a color plot
            Z = Z.reshape(xx.shape)
            ax.contourf(xx, yy, Z, cmap=cm, alpha=.8)
            
            # Plot also the training points
            ax.scatter(X_train[:, 0], X_train[:, 1], c=y_train, cmap=cm_bright,
                       edgecolors='k')
            # and testing points
            ax.scatter(X_test[:, 0], X_test[:, 1], c=y_test, cmap=cm_bright,
                       edgecolors='k', alpha=0.6)
            
            ax.set_xlim(xx.min(), xx.max())
            ax.set_ylim(yy.min(), yy.max())
            ax.set_xticks(())
            ax.set_yticks(())
            if ds_cnt == 0:
                ax.set_title(name, fontsize=24)
            ax.text(xx.max() - .3, yy.min() + .3, ('Score test: %.2f' % score).lstrip('0'),
                    size=15, horizontalalignment='right')
            i += 1
    
    plt.tight_layout()
    plt.show()
    
    
def plot_error(classifiers, datasets, hyperparameters, names):
    
    fig, axes = plt.subplots(nrows=2, ncols=2, figsize=(14, 10))
    axes = axes.ravel()
    
    for idx, data in enumerate(datasets):
        X, y = data
        X_train, X_test, y_train, y_test = train_test_split(
                X,
                y,
                test_size=.4,             # Same as in plot_classifiers
                random_state=42           # Same as in plot_classifiers
                )

        n_train, n_attributes = np.shape(X_train)

        error_train = np.empty(len(hyperparameters))
        error_test = np.empty(len(hyperparameters))


        for i, k in enumerate(hyperparameters):

            classifiers[i].fit(X_train, y_train)

            error_train[i] = 1.0 - classifiers[i].score(X_train, y_train)
            error_test[i] = 1.0 - classifiers[i].score(X_test, y_test)


        ax = axes[idx]

        ax.plot(
        hyperparameters, error_train, label='train'
        )
        ax.plot(
        hyperparameters, error_test, label='test'
        )
        ax.set_title(f"Dataset: {names[idx]}")
        ax.set_ylabel("Error")
        ax.grid(True, linestyle=':', alpha=0.6)
        ax.legend()
        
        index_min_train = np.argmin(error_train)
        err_min_train = error_train[index_min_train]
        k_min_train = hyperparameters[index_min_train]

        index_min_test = np.argmin(error_test)
        err_min_test = error_test[index_min_test]
        k_min_test = hyperparameters[index_min_test]

        print(f"--- Dataset: {names[idx]} ---")
        print('Minimun training error:\t{:5.2f} % \t k = {} '.format(100.0*err_min_train, k_min_train))
        print('Minimun test error:\t{:5.2f} %  \t k = {} '.format(100.0*err_min_test, k_min_test))

    plt.tight_layout()
    plt.show()


def plot_tree_depths(datasets, max_depth_values, names):
    """
    Genera gráficas que comparan el hiperparámetro max_depth 
    con la profundidad real alcanzada por el árbol de decisión.
    """
    
    fig, axes = plt.subplots(nrows=1, ncols=len(datasets), figsize=(5 * len(datasets), 5))
    
    for idx, data in enumerate(datasets):
        X, y = data
        
        X_train, X_test, y_train, y_test = train_test_split(
            X, y, test_size=0.4, random_state=42
        )

        real_depth = []

        for d in max_depth_values:
            clf_tree = DecisionTreeClassifier(criterion='entropy',
                                              max_depth=d,
                                              random_state=42)
            clf_tree.fit(X_train, y_train)
            
            
            real_depth.append(clf_tree.get_depth())

        ax = axes[idx]
        
        ax.plot(max_depth_values, max_depth_values, linestyle='--', color='gray', label='Límite (max_depth)')
        
        ax.plot(max_depth_values, real_depth, marker='o', color='blue', linewidth=2, label='Profundidad Real')

        ax.set_title(f"Dataset: {names[idx]}")
        ax.set_xlabel("Límite de profundidad (max_depth)")
        ax.set_ylabel("Profundidad del árbol generado")
        ax.grid(True, linestyle=':', alpha=0.6)
        ax.legend()

    plt.tight_layout()
    plt.show()

def plot_risk(df, cols_to_plot, target_col='Class', n_plots_per_row=3, figsize_width=18, figsize_height_factor=5):
    """
    Genera gráficos de barras apiladas al 100% para visualizar el riesgo (default) por categoría.
    
    Args:
        df (pd.DataFrame): El DataFrame con los datos.
        cols_to_plot (list): Lista de nombres de las columnas (atributos) a graficar.
        target_col (str): Nombre de la columna objetivo (default). Por defecto 'Class'.
                          Se asume que 1=Good, 2=Bad en los datos originales.
        n_plots_per_row (int): Número de gráficos por fila.
        figsize_width (int): Ancho total de la figura.
        figsize_height_factor (int): Altura de la figura por cada fila de gráficos.
    """
    
    # Crear una copia para no modificar el original fuera de la función
    df_plot = df.copy()
    
    # Mapeo para leyenda (Ajustar si tus datos ya son 0/1)
    # Si tus datos son 1/2:
    df_plot['Class_Label'] = df_plot[target_col].map({1: 'Good (No Default)', 2: 'Bad (Default)'})
    
    n_attributes = len(cols_to_plot)
    n_rows_fig = int(np.ceil(n_attributes / n_plots_per_row))
    
    fig, axes = plt.subplots(
        nrows=n_rows_fig, 
        ncols=n_plots_per_row, 
        figsize=(figsize_width, figsize_height_factor * n_rows_fig), 
        constrained_layout=True
    )
    
    # Manejo de casos donde hay solo 1 gráfico (axes no es lista)
    if n_attributes == 1:
        axes = [axes]
    else:
        axes = axes.flatten()
    
    # Colores: Azul para Good, Naranja para Bad
    colors = ['#1f77b4', '#ff7f0e'] 
    
    for i, col_name in enumerate(cols_to_plot):
        ax = axes[i]
        
        # 1. Crear tabla cruzada y NORMALIZAR por fila ('index')
        cross_tab = pd.crosstab(df_plot[col_name], df_plot['Class_Label'], normalize='index')
        
        # Ordenar columnas para consistencia visual
        try:
            cross_tab = cross_tab[['Good (No Default)', 'Bad (Default)']]
        except KeyError:
            pass # Si falta alguna clase, dejar como esté

        # 2. Plotear gráfico apilado
        cross_tab.plot(kind='bar', stacked=True, ax=ax, color=colors, width=0.8, edgecolor='black', alpha=0.8)
        
        # 3. Línea de referencia del 30% (Tasa media de default)
        ax.axhline(y=0.7, color='red', linestyle='--', linewidth=2, alpha=0.7, label='Media Global (30% Default)')
        
        ax.set_title(f'% de Riesgo por Categoría: {col_name}')
        ax.set_ylim(0, 1)
        ax.set_ylabel('Proporción (0 a 1)')
        ax.set_xlabel(col_name)
        
        # Poner porcentajes dentro de las barras
        for n, x in enumerate([*cross_tab.index.values]):
            if 'Bad (Default)' in cross_tab.columns:
                val_bad = cross_tab.loc[x, 'Bad (Default)']
                # Escribir el % en la parte naranja si cabe
                if val_bad > 0.05: 
                    ax.text(n, 1 - (val_bad/2), f'{val_bad:.1%}', ha='center', va='center', color='white', fontweight='bold')

        # Gestión de Leyenda (solo en el primero)
        if i == 0:
            ax.legend(loc='lower center', bbox_to_anchor=(0.5, 1.05), ncol=3)
        else:
            if ax.get_legend() is not None:
                ax.get_legend().remove()

    # Limpiar ejes vacíos
    for j in range(i + 1, len(axes)):
        axes[j].axis('off')

    plt.show()
    
    

def rendimiento_cv(model, name, X, y, cv=10):
    """
    Realiza una estimación del rendimiento en producción usando predicciones 
    'out-of-sample' generadas internamente en el Train mediante CV.
    """

    print(f"RENDIMIENTO (CV): {name}")
    
    
    y_pred_cv = cross_val_predict(model, X, y, cv=cv, n_jobs=-1)
    cm = confusion_matrix(y, y_pred_cv)
    tn, fp, fn, tp = cm.ravel()
    
    recall = tp / (tp + fn) if (tp + fn) > 0 else 0
    tasa_rechazo_buenos = fp / (fp + tn) if (fp + tn) > 0 else 0
    
    print(f"   - Defaults Identificados (TP):      {tp} de {tp+fn}")
    print(f"   - Defaults No Detectados (FN):      {fn}")
    print(f"   >>> Sensibilidad (Recall):          {recall:.2%}")
    
    print(f"\nB. Coste de oportunidad:")
    print(f"   - Falsos Positivos (Error Tipo I):  {fp}")
    print(f"   - Cartera Solvente Total:           {tn + fp}")
    print(f"   >>> Tasa de Rechazo (Solventes):    {tasa_rechazo_buenos:.2%}")
    
    return recall, tasa_rechazo_buenos

def examen_test(model, X_test, y_test):
    """
    Reporte final de texto plano con alineación tabular.
    """
    # 1. Predicción y extracción de datos
    y_pred_test = model.predict(X_test)
    cm = confusion_matrix(y_test, y_pred_test)
    tn, fp, fn, tp = cm.ravel()
    
    # 2. Variables para el reporte
    total_defaults = tp + fn
    total_solventes = tn + fp
    
    recall = recall_score(y_test, y_pred_test)
    f2 = fbeta_score(y_test, y_pred_test, beta=2)
    acc = accuracy_score(y_test, y_pred_test)
    tasa_rechazo = (fp + tp) / (tn + fp + fn + tp)
    
    print(f"\n1. DEFAULTS:")
    print()
    print(f"   {'Defaults Reales en Test':<35} : {total_defaults:>10}")
    print(f"   {'Defaults Detectados (TP)':<35} : {tp:>10}")
    print(f"   {'Defaults FUGADOS (FN)':<35} : {fn:>10}")
    print(f"   {'> SENSIBILIDAD (Recall)':<35} : {recall:>10.2%}")
    print(f"   {'> F2-SCORE':<35} : {f2:>10.2%}")

    print(f"\n2. CLIENTES BUENOS PERDIDOS:")
    print()
    print(f"   {'Clientes Solventes Totales':<35} : {total_solventes:>10}")
    print(f"   {'Rechazos Incorrectos (FP)':<35} : {fp:>10}")
    print(f"   {'> TASA DE RECHAZO (Global)':<35} : {tasa_rechazo:>10.2%}")
    print(f"   {'> ACCURACY':<35} : {acc:>10.2%}")
    

def confussion_matrix(model_obj, X, y, model_name, cv=5, mode='cv'):
    """
    Función Universal para graficar matrices de confusión.
    
    Parámetros:
    - mode='cv':   Usa cross_val_predict (Para Fases de Selección/Entrenamiento).
                   Es una simulación "honesta" sin tocar el Test Set.
    - mode='test': Usa predict directo (Para la Auditoría Final).
                   Usa el modelo ya entrenado sobre datos nuevos.
    """
    
    # 1. GENERACIÓN DE PREDICCIONES (Según el modo)
    if mode == 'cv':
        print(f"Generando matriz CV para: {model_name}...")
        # Simula predicciones honestas re-entrenando por folds
        y_pred = cross_val_predict(model_obj, X, y, cv=cv, n_jobs=-1)
        subtitle = "(Validación Cruzada)"
        
    elif mode == 'test':
        print(f"Generando matriz TEST para: {model_name}...")
        # Predicción directa con el modelo ya entrenado
        y_pred = model_obj.predict(X)
        subtitle = "(Examen Test)"
        
    # 2. VISUALIZACIÓN (Código compartido)
    plt.figure(figsize=(6, 5))
    ConfusionMatrixDisplay.from_predictions(
        y, y_pred,
        display_labels=['Solvente', 'Default'],
        cmap='Reds',          
        colorbar=False,       
        values_format='d',    
        text_kw={'fontsize': 14, 'fontweight': 'bold'} 
    )
    
    plt.title(f"Matriz de Impacto: {model_name}\n{subtitle}", fontsize=13, pad=15)
    plt.ylabel('Realidad (Etiqueta Verdadera)', fontsize=11)
    plt.xlabel('Decisión del Modelo (Predicción)', fontsize=11)
    plt.tight_layout()
    plt.show()


def cross_validation(model_class, param_name, param_range, X, y, static_params={}, model_name="Modelo"):
    """
    Realiza una validación cruzada iterativa variando un hiperparámetro clave.
    devuelve el mejor valor encontrado para poder usarlo después automáticamente.
    """
    # Definimos el scorer dentro por si acaso no es global
    f2_scorer = make_scorer(fbeta_score, beta=2)
    
    train_scores = []
    cv_scores = []
    x_plot = []
    
    # Bucle de prueba
    for val in param_range:
        # Configuración dinámica
        params = static_params.copy()
        params[param_name] = val
        
        # Instancia del modelo
        clf = model_class(**params)
        
        # A) Score Validación (Realista - F2)
        scores = cross_val_score(clf, X, y, cv=5, scoring=f2_scorer, n_jobs=-1)
        cv_scores.append(scores.mean())
        
        # B) Score Train (Memorización - F2)
        clf.fit(X, y)
        y_pred_train = clf.predict(X)
        train_score = fbeta_score(y, y_pred_train, beta=2)
        train_scores.append(train_score)
        
        # Manejo eje X
        x_plot.append(str(val) if val is not None else "None")

    # Selección del Ganador
    best_idx = np.argmax(cv_scores)
    best_val = param_range[best_idx]
    best_score = cv_scores[best_idx]
    
    # Gráfico
    plt.figure(figsize=(10, 5))
    indices = range(len(x_plot))
    
    plt.plot(indices, train_scores, 'o--', color='blue', label='Train (Memorización)', alpha=0.6)
    plt.plot(indices, cv_scores, 'o-', color='green', label='CV (Validación Real)', linewidth=2)
    
    # Línea del óptimo
    plt.axvline(best_idx, color='red', linestyle=':', label=f'Mejor Config: {param_name}={best_val}')
    
    plt.xticks(indices, x_plot, rotation=45 if len(x_plot) > 15 else 0)
    plt.xlabel(f'Hiperparámetro: {param_name}')
    plt.ylabel('F2-Score (Riesgo)')
    plt.title(f'Optimización: {model_name}\n(Máximo Rendimiento: {best_score:.2%})')
    plt.legend()
    plt.grid(True, alpha=0.3)
    plt.show()
    
    print(f"Ganador: {param_name} = {best_val} -> F2: {best_score:.2%}\n")
    
    return best_val