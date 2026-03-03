# PRÁCTICA 3 - SEMINARIO MÉTODOS CUANTITATIVOS

"""
Machine learning: Exercise sheet 1

Auxiliary function for plotting the results of the classsifiers

This is a temporary script file.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.metrics import recall_score, balanced_accuracy_score
from sklearn.metrics import accuracy_score



def plot_class_distribution(df, col='Sentiment'):
    # Definir etiquetas explícitamente para mantener el orden y limpieza
    labels = ['negative', 'somewhat\nnegative', 'neutral', 'somewhat\npositive', 'positive']
    
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 5))

    # Cálculo de datos
    counts = df[col].value_counts().sort_index()
    props = df[col].value_counts(normalize=True).sort_index()

    # Gráfico 1: Conteo (Absoluto)
    counts.plot(kind='bar', ax=ax1, color='steelblue')
    ax1.set_title('Distribución de Clases (Conteo)', fontsize=12, fontweight='bold')
    ax1.set_ylabel('Frecuencia')

    # Gráfico 2: Porcentaje (Relativo)
    props.plot(kind='bar', ax=ax2, color='coral')
    ax2.set_title('Distribución de Clases (Porcentaje)', fontsize=12, fontweight='bold')
    ax2.set_ylabel('Proporción')

    # Configuración común para ambos ejes (limpieza de código)
    for ax in [ax1, ax2]:
        ax.set_xlabel(col, fontsize=10)
        ax.set_xticklabels(labels, rotation=0, ha='center')
        ax.grid(axis='y', alpha=0.3)

    plt.tight_layout()
    plt.show()


def report_prediction_distribution(y_true, y_pred, target_names):
    """
    Imprime un reporte comparando la distribución de las predicciones vs la realidad.
    
    Args:
        y_true: Array/Series con las etiquetas reales.
        y_pred: Array/Series con las etiquetas predichas.
        target_names: Lista de strings con los nombres de las clases (en orden).
    """
    print("ANÁLISIS DE LA MATRIZ DE CONFUSIÓN")
    
    s_pred = pd.Series(y_pred)
    s_true = pd.Series(y_true)
    all_classes = range(len(target_names))

    # --- BLOQUE 1: PREDICCIONES ---
    print("\nDistribución de predicciones del modelo:")
    pred_counts = s_pred.value_counts().reindex(all_classes, fill_value=0)
    total_preds = len(s_pred)

    for i in all_classes:
        count = pred_counts[i]
        percentage = (count / total_preds) * 100
        print(f"   Clase {i} ({target_names[i]}): {int(count)} predicciones ({percentage:.1f}%)")

    # --- BLOQUE 2: REALIDAD ---
    print("\nDistribución real en el conjunto de test:")
    real_counts = s_true.value_counts().reindex(all_classes, fill_value=0)
    total_real = len(s_true)

    for i in all_classes:
        count = real_counts[i]
        percentage = (count / total_real) * 100
        print(f"   Clase {i} ({target_names[i]}): {int(count)} ejemplos ({percentage:.1f}%)")
        

def compare_models_performance(y_true, predictions_dict, classes_of_interest=[0, 4]):
    """
    Genera un cuadro de mando visual comparando la capacidad de detección de extremos.
    """
    results = []
    class_low, class_high = classes_of_interest
    
    # Cálculo de métricas
    for model_name, y_pred in predictions_dict.items():
        # Recall por clase (average=None devuelve array)
        recalls = recall_score(y_true, y_pred, average=None)
        bal_acc = balanced_accuracy_score(y_true, y_pred)
        
        results.append({
            'Modelo': model_name,
            f'Recall Clase {class_low} (Negativo)': recalls[class_low],
            f'Recall Clase {class_high} (Positivo)': recalls[class_high],
            'Balanced Accuracy': bal_acc
        })
    
    df_results = pd.DataFrame(results)
    
    # 2. VISUALIZACIÓN
    fig, axes = plt.subplots(1, 2, figsize=(16, 6))
    
    # Gráfico A: Detección de Extremos (El gráfico clave)
    x = np.arange(len(df_results))
    width = 0.35
    
    # Barras Rojas (Negativo) y Verdes (Positivo)
    rects1 = axes[0].bar(x - width/2, df_results[f'Recall Clase {class_low} (Negativo)'], width, 
                label=f'Detectar Negativos (Clase {class_low})', color='#d62728', alpha=0.9, edgecolor='black')
    rects2 = axes[0].bar(x + width/2, df_results[f'Recall Clase {class_high} (Positivo)'], width, 
                label=f'Detectar Positivos (Clase {class_high})', color='#2ca02c', alpha=0.9, edgecolor='black')
    
    axes[0].set_ylabel('Sensibilidad (Recall)', fontweight='bold', fontsize=11)
    axes[0].set_title('CAPACIDAD DE DETECCIÓN DE EXTREMOS', fontweight='bold', fontsize=12)
    axes[0].set_xticks(x)
    axes[0].set_xticklabels(df_results['Modelo'], rotation=15, ha='right')
    axes[0].legend()
    axes[0].grid(axis='y', alpha=0.3, linestyle='--')
    axes[0].set_ylim([0, 0.8]) # Ajustamos límite para ver bien las barras

    # Añadir valores
    for rects in [rects1, rects2]:
        for rect in rects:
            height = rect.get_height()
            axes[0].annotate(f'{height:.2f}',
                        xy=(rect.get_x() + rect.get_width() / 2, height),
                        xytext=(0, 3), textcoords="offset points",
                        ha='center', va='bottom', fontweight='bold', fontsize=9)
    
    # Gráfico B: Balanced Accuracy (Visión Global)
    bars = axes[1].bar(df_results['Modelo'], df_results['Balanced Accuracy'], 
                color=['#7f7f7f', '#ff7f0e', '#1f77b4'], alpha=0.9, edgecolor='black')
    
    axes[1].set_ylabel('Balanced Accuracy', fontweight='bold', fontsize=11)
    axes[1].set_title('RENDIMIENTO GLOBAL EQUILIBRADO', fontweight='bold', fontsize=12)
    axes[1].grid(axis='y', alpha=0.3, linestyle='--')
    axes[1].set_ylim([0, 0.6])
    axes[1].set_xticklabels(df_results['Modelo'], rotation=15, ha='right')

    for bar in bars:
        height = bar.get_height()
        axes[1].text(bar.get_x() + bar.get_width()/2., height + 0.01,
                f'{height:.4f}', ha='center', va='bottom', fontweight='bold', fontsize=10)
        
    plt.tight_layout()
    plt.show()
    return df_results


def metrics(y_true, y_pred, model_name, benchmark_score=None):
    """
    Calcula, imprime y devuelve métricas clave.
    
    Args:
        benchmark_score: Si es None, asume que este es el Baseline y calcula el sesgo (Acc - Bal Acc).
                         Si es un float, calcula la mejora respecto a ese valor (Bal Acc - Benchmark).
    Returns:
        float: El Balanced Accuracy del modelo actual (para usarlo como benchmark futuro).
    """
    acc = accuracy_score(y_true, y_pred)
    bal_acc = balanced_accuracy_score(y_true, y_pred)
    
    print(f"\n--- MÉTRICAS {model_name.upper()} ---")
    print(f"   Accuracy:          {acc:.4f}")
    print(f"   Balanced Accuracy: {bal_acc:.4f}")
    
    if benchmark_score is None:
        # Caso Baseline: Analizamos sesgo interno
        gap = abs(acc - bal_acc)
        print(f"   Diferencia (Sesgo): {gap:.4f}")
        print("   (La diferencia indica el sesgo hacia la clase mayoritaria)")
    else:
        # Caso Comparativo: Analizamos mejora externa
        improvement = bal_acc - benchmark_score
        print(f"   Mejora vs Baseline: {improvement:+.4f}") # El + fuerza el signo
        
    return bal_acc


def ranking_final(df, figsize=(6, 4)):
    
    plt.figure(figsize=figsize)
    
    # Verde para el máximo, Gris para el resto
    max_score = df['F1-Macro'].max()
    colors = ['#2ca02c' if x == max_score else '#7f7f7f' for x in df['F1-Macro']]
    
    # Gráfico
    bars = plt.bar(df['Modelo'], df['F1-Macro'], color=colors, edgecolor='black', alpha=0.9)
    
    # Estética
    plt.ylabel('F1-Score Macro (Rentabilidad)', fontweight='bold', fontsize=9)
    plt.title('RANKING FINAL: Selección del Modelo', fontweight='bold', fontsize=11, pad=15)
    plt.grid(axis='y', alpha=0.3, linestyle='--')
    plt.ylim(0, max_score * 1.25)
    
    # Etiquetas de valor
    for bar in bars:
        height = bar.get_height()
        plt.text(bar.get_x() + bar.get_width()/2., height + 0.005,
                 f'{height:.4f}', ha='center', va='bottom', fontweight='bold', fontsize=9)
    
    plt.tight_layout()
    plt.show()