# -*- coding: utf-8 -*-
"""
Machine learning: Exercise sheet 1

Auxiliary function for plotting the results of the classsifiers

This is a temporary script file.
"""

import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from matplotlib.colors import ListedColormap
from sklearn.tree import DecisionTreeClassifier


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
