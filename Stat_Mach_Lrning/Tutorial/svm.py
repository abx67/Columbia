from __future__ import print_function
import sys
import numpy as np
import numpy.random as npr

def sgdStepWeight():
    i = 0.0
    while True:
        i += 1.0
        yield i

def svm_gd(y, X, C, eps=0.25, eta = 18e-4, fig=False, MAX_ITER = 500):
    '''
    This function implement SVM by gradient descent
    Argument:
        y: response
        X: input
        C: cost
        fig: whether or not plot figures, default False
    Value:
        w, b: separating hyperplane f(x) = <w, x> + b
    '''

    # --- initialization
    n, d = X.shape
    w = np.zeros(d, dtype=np.float64)
    b = 0.0
    itr = 0  # iteration count
    
    # --- objective function
    obj = C
    obj0 = obj / 2.0  # to pass the first loop
    
    # gradient vector
    grad_w_tmp = -X.T * y
    grad_b_tmp = -y

    # -- update objTrack
    if fig is True:
        objTrack = np.zeros(MAX_ITER)
        objTrack[itr] = obj

    # --- update iteration
    keyCond = np.maximum(0.0, 1.0 - y * (np.dot(X, w) + b))

    while np.absolute((obj - obj0)) / obj > eps / 100.0:
        obj0 = obj
        # update w, b
        w = (1 - eta) * w - eta * C * np.sum(grad_w_tmp[:, keyCond > 0.0], axis=1) / n
        b -= eta * C * np.sum(grad_b_tmp[keyCond > 0.0]) / n
        # update keyCond
        keyCond = np.maximum(0.0, 1.0 - y * (np.dot(X, w) + b))
        # update objective function
        obj  = np.sum(w ** 2) / 2.0 / n + C * np.mean(keyCond)  # objective function

        itr += 1
        # update objTrack
        if fig is True:
            objTrack[itr] = obj
        if itr >= MAX_ITER - 1: # check MAX_ITER
            print("max iteration {0} reached, quit iteration.".format(MAX_ITER), file=sys.stderr)
            break

    if fig is True:
        return (w, b, objTrack[: itr + 1])
    return (w, b)

def svm_sgd(y, X, C, eta0=1.2e-2, eps=0.001, fig=False, MAX_ITER=30000):
    '''
    This function implement SVM by stochastic gradient descent
    Argument:
        y: response
        X: input
        C: cost
        fig: whether or not plot figures, default False
    Value:
        w, b: separating hyperplane f(x) = <w, x> + b
    '''
    # --- initialization
    n, d = X.shape
    w  = np.zeros(d)
    b  = 0
    index = npr.permutation(n) # permute index; alternative: npr.choice()
    
    # --- control parameters
    Delta = 0
    itr = 0  # iteration count
    wgt = sgdStepWeight()
    
    ## --- objective function
    obj = C
    obj0 = obj / 2.0  # to pass the first loop
    
    # gradient vector
    grad_w_tmp = - X.T * y
    grad_b_tmp = - y
    
    # -- update objTrack
    if fig is True:
        objTrack = np.zeros(MAX_ITER)
        objTrack[itr] = obj

    ## --- update iteration
    keyCond = np.maximum(0, 1 - y * (np.dot(X, w) + b))
    Delta = 0.5 * Delta + 0.5 * np.absolute((obj - obj0)) / obj * 100.0
    eta = eta0 / wgt.next()

    while Delta > eps:
        obj0 = obj
        
        # update w, b
        w = (1 - eta) * w
        if keyCond[index[itr % n]] > 0:
            w -= eta * C * grad_w_tmp[:, index[itr % n]]
            b -= eta * C * grad_b_tmp[index[itr % n]]
        # update keyCond
        keyCond = np.maximum(0, 1 - y * (np.dot(X, w) + b))
        # update objective function
        obj = np.sum(w ** 2) / 2.0 / n + C * np.mean(keyCond)  # objective function

        itr += 1
        Delta = 0.5 * Delta + 0.5 * np.absolute((obj - obj0)) / obj * 100.0
        eta = eta0 / wgt.next()

        if fig is True: # update objTrack
            objTrack[itr] = obj

        if itr >= MAX_ITER - 1: # check MAX_ITER
            print("max iteration {0} reached, quit iteration.".format(MAX_ITER), file=sys.stderr)
            break

    if fig is True:
        return (w, b, objTrack[: itr + 1])

    return (w, b)

def svm_mnBat(y, X, C, eta0=1.3e-2, eps=0.01, batch_size=20, fig=False, MAX_ITER=3000):
    '''
    this function implement SVM by mini-batch stochastic gradient descent
    argument:
        y: response
        X: input
        C: cost
        fig: whether or not plot figures, default False
    value:
        w, b: separating hyperplane f(x) = <w, x> + b
    '''
    # initialization
    n, d = X.shape
    w = np.zeros(d)
    b = 0
    index = npr.permutation(n)
        
    # control parameters
    Delta = 0.0
    itr = 0  # iteration count
    i = 0
    wgt = sgdStepWeight()
 
    # objective value
    obj = C
    obj0 = obj / 2.0  # to pass the first loop
    
    # gradient vector
    grad_w_tmp = (-X.T * y)[:, index]
    grad_b_tmp = -y[index]
    
    # initialize objTrack
    if fig is True:
        objTrack = np.zeros(MAX_ITER)
        objTrack[itr] = obj

    # update iteration
    keyCond = np.maximum(0.0, 1.0 - y * (np.dot(X, w) + b))[index]
    Delta = 0.5 * Delta + 0.5 * np.absolute((obj - obj0)) / obj * 100.0
    eta = eta0 / wgt.next()

    while Delta > eps:
        obj0 = obj
        # update w, b
        w = (1 - eta) * w
        w -= eta * C * np.sum(grad_w_tmp[:, i : min(i + batch_size, n)][:, keyCond[i : min(i + batch_size, n)] > 0], axis=1) / batch_size
        b -= eta * C * np.sum(grad_b_tmp[i : min(i + batch_size, n)][keyCond[i : min(i + batch_size, n)] > 0]) / batch_size
        
        # update keyCond
        keyCond = np.maximum(0, 1 - y * (np.dot(X, w) + b))[index]

        # update objective value
        obj  = np.sum(w ** 2) / 2.0 / n + C * np.mean(keyCond)  # objective function

        itr += 1
        i += batch_size
        if i >= n:
            i = 0
        
        Delta = 0.5 * Delta + 0.5 * np.absolute((obj - obj0)) / obj * 100.0
        eta = eta0 / wgt.next()
        
        # update objTrack
        if fig is True:
            objTrack[itr] = obj
        
        # check MAX_ITER
        if itr >= MAX_ITER - 1:
            print("max iteration {0} reached, quit iteration.".format(MAX_ITER), file=sys.stderr)
            break

    if fig is True:
        return (w, b, objTrack[: itr + 1])

    return (w, b)

