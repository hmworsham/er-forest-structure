
# Parameters:

# 1. ret_wf = return waveform
# 2. out_wf = outgoing waveform
# 3. imp_resp = return impulse response or system impulse response, if the system response has been lab-calibrated
# 4. imp_out = outgoing pulse of the return impulse response
# 5. method = specifies the Gold or Richardson-Lucy algorithms['Gold', 'RL']
# default = 'Gold'
# 6. param_thresh = threshold for determining whether small or big parameters should be used
# default = 2
# 7. rescale = whether to rescale waveform intensity
# default = True
# 8. small_params = iterations, repetitions, and boost when the waveform is simple with little noise
# 9. big_params = iterations, repetitions, and boost for deconvolution when waveform is complicated
# - n_iters = number of iterations between boosting operatoins in the Gold deconvolution
# - n_reps = number of repetitions of boosting operations
# must be >= 1
# - boosting_coef = applies only if n_reps > 1
# recommended range[1, 2]
# 10. imp_out_pars = deconvolution parameters for obtaining system impluse response using the impulse response and corresponding outgoing pulse
# 11. return = deconvolved waveform


series = [2, 2, 2, 2, 4, 5, 7, 8, 12, 9, 7, 5, 2, 2, 2, 4, 5,
          6, 12, 17, 18, 19, 19, 18, 17, 15, 12, 6, 5, 4, 2, 2, 2]
span = 3

def embed(series, span):
    span_indices = np.flip(np.arange(span))
    embed_array = []
    for j in np.arange(len(series))[span:]:
        embed_s = series[j-span:j]
        embed_s.reverse()
        embed_array.append(embed_s)

    return embed_array


def lpeak(series, span= 3):
    '''
    @param series is the input a numeric vector.
#' @param span is the length or interval of peak finding cell, default is 3.
#' @return return a boolean type data corresponding to the numeric vector.
'''
    series = [float(i) for i in series]
    z = embed(series, span)
    s = span // 2
    v = np.argmax(z, axis=1) == s
    falses = np.full(s, False)
    result = np.append(falses, v)
    result = result[:(len(result)-s)]
    return result


def n_peaks(y, drop=[0, 0], smooth=True, smooth_window=3, threshold=0.2):
    y = [float(i) for i in y]
    y = [np.nan if i == 0 else i for i in y]
    if drop[0] > 0:
        y = np.delete(y, np.arange(drop[0], drop[1]))

    # quick rescale
    y = y - np.nanmin(y) + 1

    if smooth == True:
        inc = smooth_window
        y_smooth = np.array([np.nanmean(y[idx:idx+inc])
                             for idx in range(len(y))])
        nan_idxs = np.where(np.isnan(y))[0]
        y_smooth[nan_idxs] = np.nan
        y = y_smooth

    peakrecord = lpeak(y, 3)  # show True and False
    peaknumber = np.argwhere(peakrecord == True)  # find index of True values
    imax = np.nanmax(y)
    ind = y[peaknumber] > threshold * imax
    realind = peaknumber[ind]
    newpeak = y[realind]
    z = len(realind)

    return z



def deconv_gold(source, response, n_iter = -1, n_reps = -1, boost = 0):
    
    # strip trailing zeros from source and response
    source = np.array([float(i) for i in np.trim_zeros(source, 'b')])
    response = np.array([float(i) for i in np.trim_zeros(response, 'b')])

    return source, response

    if len(response) < len(source):
        response = np.append(response, np.full(len(source) - len(response), 0))
    if len(response) > len(source):
        print('ERROR: length of response must be <= length of y')

    # make sure we have ssize and n_reps specified
    ssize = len(source)

    if ssize <= 0:
        print('ERROR: Incorrect source length')

    if n_reps <= 0:
        print('ERROR: Incorrect source length')
    
    # define a working space 4x the length of the source
    working_space = np.full(4 * ssize, 0.99999)
    
    # define initial values of some parameters in the Gold algorithm 
    posit = 0
    lh_gold = -1
    area = 0
    maximum = 0

    # read response vector
    for i in range(ssize):
        lda = response[i]
        if lda != 0:
            lh_gold = i+1
        working_space[i] = lda
        area += lda
        if lda > maximum:
            maximum = lda
            posit = i
    if lh_gold == -1:
        print('ERROR: Zero response vector')

    # read source vector
    for i in range(ssize):
        working_space[2 * ssize + i] = source[i]

    # create matrix at *x and vector at *y
    for i in range(ssize):
        lda = 0
        for j in range(ssize):
            ldb = working_space[j]
            k = i + j
            if k < ssize:
                ldc = working_space[k]
                lda = lda + ldb * ldc
        working_space[ssize + i] = lda
        lda = 0
        for k in range(ssize):
            l = k - i
            if l >= 0:
                ldb = working_space[l]
                ldc = working_space[2 * ssize + k]
                lda = lda + ldb * ldc
                #print(lda)
        working_space[3 * ssize + i] = lda

    # move vector at *y
    for i in range(ssize):
        working_space[2 * ssize + i] = working_space[3 * ssize + i]
    #print('move vector', working_space)

    # initialize result vector
    for i in range(ssize):
        working_space[i] = 1

    # start iterations
    for rep in range(n_reps):
        if rep != 0:
            for i in range(ssize):
                working_space[i] = working_space[i] ** boost
        for lindex in range(n_iter):
            for i in range(ssize):
                if (working_space[2 * ssize + i] > 0.000001) & (working_space[i] > 0.000001):
                    lda = 0
                    for j in range(lh_gold):
                        ldb = working_space[j + ssize]
                        if j != 0:
                            k = i + j
                            ldc = 0
                            if k < ssize:
                                ldc = working_space[k]
                            k = i - j
                            if k >= 0:
                                ldc += working_space[k]
                        else:
                            ldc = working_space[i]
                        lda = lda + ldb * ldc
                    ldb = working_space[2 * ssize + i]
                    if lda != 0:
                        lda = ldb / lda
                    else:
                        lda = 0
                    ldb = working_space[i]
                    lda = lda * ldb
                    working_space[3 * ssize + i] = lda
            for i in range(ssize):
                working_space[i] = working_space[3 * ssize + i]
    
    # shift and write back resulting spectrum
    f = np.full(ssize,np.nan)
    for i in range(ssize):
        lda = working_space[i]
        j = i + posit
        j = j % ssize
        f[j] = lda
    
    return f


def SpectrumDeconvolution(y, response, iterations= 10, repetitions =1, boost = 1.5, method = 'Gold'):

    if method == 'Gold':
        gold = deconv_gold(y, response, iterations, repetitions, boost)
        return gold
    else:
        lr = deconv_lr(y, response, iterations, repetitions, boost)
        return lr






def deconvolution(ret_wf, out_wf, imp_resp = None, imp_out = None, method = None, param_thresh=2, rescale = True, small_params=[30,2,1.8,30,2,1.8], big_params=[30,3,1.8,40,3,1.8], imp_out_params=[20,5,1.8]):
    '''
    deconvolve a return waveform by its corresponding outgoing pulse and system impulse response
    '''
    # define x, y as return and outgoing waveforms
    x, y = np.array([float(i) for i in out_wf]), np.array([float(i) for i in ret_wf])
    
    if imp_resp is not None:
        imp_resp = np.array([float(i) for i in imp_resp])
        
    if imp_out is not None:
        imp_out = np.array([float(i) for i in imp_out])

    # rescale if necessary
    if rescale == True:
        y[y == 0] = np.nan
        y = y - np.nanmin(y) + 1
        y[np.isnan(y)] = 0

        x[x==0]
        x = x - np.nanmin(x) + 1
        x[np.isnan(x)] = 0

        if imp_resp is not None:
            imp_resp[imp_resp == 0 ] = np.nan
            minimp = np.nanmin(imp_resp)
            imp_resp = imp_resp - minimp+1
            imp_resp[np.isnan(imp_resp)] = 0
    
    zn = n_peaks(y)

    if zn <= param_thresh:
        irb1 = small_params[0:3]
        irb2 = small_params[3:6]
    else:
        irb1 = large_params[0:3]
        irb2 = large_params[3:6]

    if imp_out is not None and len(imp_out) > 0:
        imp_out[imp_out == 0] = np.nan
        imp_out = imp_out - np.nanmin(imp_out)
        imp_out[np.isnan(imp_out)] = 0
    
        # deconvolve impulse response using impulse response's corresponding out pulse within peaks function
        imre = SpectrumDeconvolution(
            imp_resp, 
            imp_out, 
            iterations = imp_out_params[0], 
            repetitions = imp_out_params[1],
            boost = imp_out_params[2],
            method = method
            )
        imre = np.array([round(i, 2) for i in imre]) # rounding to two decimals is very important

        # deconvolve return pulse using impulse response within peaks function
        y1 = SpectrumDeconvolution(
            y, 
            imre,
            iterations = irb1[0],
            repetitions = irb1[1],
            boost = irb1[2],
            method = method
            )
    
    else:
        y1 = SpectrumDeconvolution(
            y,
            imp_resp,
            iterations = irb1[0],
            repetitions = irb1[1],
            boost = irb1[2],
            method = method
            )

    de = SpectrumDeconvolution(
        y1,
        x,
        iterations = irb2[0],
        repetitions = irb2[1],
        boost = irb2[2],
        method = method
        )
    
    de_round = [round(i, 2) for i in de]

    return de_round