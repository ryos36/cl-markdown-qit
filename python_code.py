```python:test.py
# +--------------------------------------------------------------------------+
# | CHStone : a suite of benchmark programs for C-based High-Level Synthesis |
# | ======================================================================== |
# |                                                                          |
# | * Collected and Modified : Y. Hara, H. Tomiyama, S. Honda,               |
# |                            H. Takada and K. Ishii                        |
# |                            Nagoya University, Japan                      |
# |                                                                          |
# | * Remark :                                                               |
# |    1. This source code is modified to unify the formats of the benchmark |
# |       programs in CHStone.                                               |
# |    2. Test vectors are added for CHStone.                                |
# |    3. If "main_result" is 0 at the end of the program, the program is    |
# |       correctly executed.                                                |
# |    4. Please follow the copyright of each benchmark program.             |
# +--------------------------------------------------------------------------+
# */
# /*************************************************************************/
# /*                                                                       */
# /*   SNU-RT Benchmark Suite for Worst Case Timing Analysis               */
# /*   =====================================================               */
# /*                              Collected and Modified by S.-S. Lim      */
# /*                                           sslim@archi.snu.ac.kr       */
# /*                                         Real-Time Research Group      */
# /*                                        Seoul National University      */
# /*                                                                       */
# /*                                                                       */
# /*        < Features > - restrictions for our experimental environment   */
# /*                                                                       */
# /*          1. Completely structured.                                    */
# /*               - There are no unconditional jumps.                     */
# /*               - There are no exit from loop bodies.                   */
# /*                 (There are no 'break' or 'return' in loop bodies)     */
# /*          2. No 'switch' statements.                                   */
# /*          3. No 'do..while' statements.                                */
# /*          4. Expressions are restricted.                               */
# /*               - There are no multiple expressions joined by 'or',     */
# /*                'and' operations.                                      */
# /*          5. No library calls.                                         */
# /*               - All the functions needed are implemented in the       */
# /*                 source file.                                          */
# /*                                                                       */
# /*                                                                       */
# /*************************************************************************/
# /*                                                                       */
# /*  FILE: adpcm.c                                                        */
# /*  SOURCE : C Algorithms for Real-Time DSP by P. M. Embree              */
# /*                                                                       */
# /*  DESCRIPTION :                                                        */
# /*                                                                       */
# /*     CCITT G.722 ADPCM (Adaptive Differential Pulse Code Modulation)   */
# /*     algorithm.                                                        */
# /*     16khz sample rate data is stored in the array test_data[SIZE].    */
# /*     Results are stored in the array compressed[SIZE] and result[SIZE].*/
# /*     Execution time is determined by the constant SIZE (default value  */
# /*     is 2000).                                                         */
# /*                                                                       */
# /*  REMARK :                                                             */
# /*                                                                       */
# /*  EXECUTION TIME :                                                     */
# /*                                                                       */
# /*                                                                       */
# /*************************************************************************/

import polyphony
from adpcm_common import *

# G722 C code


class ADPCM_Encoder:
    def __init__(self):
        # variables for transimit quadrature mirror filter here
        self.tqmf = [0] * 24

        self.delay_dhx = [0] * 6
        self.delay_bph = [0] * 6
        self.delay_bpl = [0] * 6
        self.delay_dltx = [0] * 6

        self.deth = 8

        self.nbl = 0  # delay line
        self.al1 = 0
        self.al2 = 0
        self.plt1 = 0
        self.plt2 = 0
        self.rlt1 = 0
        self.rlt2 = 0

        self.detl = 32
        self.nbh = 0

        self.ah1 = 0
        self.ah2 = 0
        self.ph1 = 0
        self.ph2 = 0
        self.rh1 = 0
        self.rh2 = 0

    # G722 encode function two ints in, one 8 bit output
    def encode(self, xin1, xin2) -> int:
        # main multiply accumulate loop for samples and coefficients
        xa = 0
        xb = 0
        for i in range(0, 24, 2):
            xa += self.tqmf[i] * COEFFS[i]
            xb += self.tqmf[i + 1] * COEFFS[i + 1]

        # update delay line tqmf
        #for i in range(23, 1, -1):
        #    self.tqmf[i] = self.tqmf[i - 2]
        i = 0
        tqmf_ptr1 = 23
        tqmf_ptr2 = 21
        while i < 22:
            self.tqmf[tqmf_ptr1] = self.tqmf[tqmf_ptr2]
            tqmf_ptr1 -= 1
            tqmf_ptr2 -= 1
            i += 1
        self.tqmf[1] = xin1
        self.tqmf[0] = xin2

        # scale outputs
        xl = (xa + xb) >> 15
        xh = (xa - xb) >> 15
        
        # end of quadrature mirror filter code

        # starting with lower sub band encoder

        # filtez - compute predictor output section - zero section
        szl = filtez(self.delay_bpl, self.delay_dltx)

        # filtep - compute predictor output signal (pole section)
        spl = filtep(self.rlt1, self.al1, self.rlt2, self.al2)
        
        # compute the predictor output value in the lower sub_band encoder
        sl = szl + spl
        el = xl - sl
        
        # quantl: quantize the difference signal
        il = quantl(el, self.detl)
        
        # computes quantized difference signal
        # for invqbl, truncate by 2 lsbs, so mode = 3
        dlt = (self.detl * qq4_code4_table[il >> 2]) >> 15

        # logscl: updates logarithmic quant. scale factor in low sub band
        self.nbl = logscl(il, self.nbl)

        # scalel: compute the quantizer scale factor in the lower sub band
        # calling parameters nbl and 8 (constant such that scalel can be scaleh)
        self.detl = scalel(self.nbl, 8)

        # parrec - simple addition to compute recontructed signal for adaptive pred
        plt = dlt + szl

        # upzero: update zero section predictor coefficients (sixth order)
        # calling parameters: dlt, dlt1, dlt2, ..., dlt6 from dlt
        #  bpli (linear_buffer in which all six values are delayed
        # return params:      updated bpli, delayed dltx
        upzero(dlt, self.delay_dltx, self.delay_bpl)

        # uppol2- update second predictor coefficient apl2 and delay it as al2
        # calling parameters: al1, al2, plt, plt1, plt2
        #global al1, al2
        self.al2 = uppol2(self.al1, self.al2, plt, self.plt1, self.plt2)

        # uppol1 :update first predictor coefficient apl1 and delay it as al1
        # calling parameters: al1, apl2, plt, plt1
        self.al1 = uppol1(self.al1, self.al2, plt, self.plt1)

        # recons : compute recontructed signal for adaptive predictor
        rlt = sl + dlt

        # done with lower sub_band encoder; now implement delays for next time
        self.rlt2 = self.rlt1
        self.rlt1 = rlt
        self.plt2 = self.plt1
        self.plt1 = plt

        # high band encode
        szh = filtez(self.delay_bph, self.delay_dhx)
        sph = filtep(self.rh1, self.ah1, self.rh2, self.ah2)

        # predic: sh = sph + szh
        sh = sph + szh
        # subtra: eh = xh - sh
        eh = xh - sh

        # quanth - quantization of difference signal for higher sub-band
        # quanth: in-place for speed params: eh, deth (has init. value)
        if eh >= 0:
            ih = 3			# 2,3 are pos codes
        else:
            ih = 1			# 0,1 are neg codes
        decis = (564 * self.deth) >> 12
        if abs(eh) > decis:
            ih -= 1			# mih = 2 case

        # compute the quantized difference signal, higher sub-band
        dh = (self.deth * qq2_code2_table[ih]) >> 15

        # logsch: update logarithmic quantizer scale factor in hi sub-band
        self.nbh = logsch(ih, self.nbh)

        # note : scalel and scaleh use same code, different parameters
        self.deth = scalel(self.nbh, 10)

        # parrec - add pole predictor output to quantized diff. signal
        ph = dh + szh

        # upzero: update zero section predictor coefficients (sixth order)
        # calling parameters: dh, dhi, bphi
        # return params: updated bphi, delayed dhx
        upzero(dh, self.delay_dhx, self.delay_bph)

        # uppol2: update second predictor coef aph2 and delay as ah2
        # calling params: ah1, ah2, ph, ph1, ph2
        self.ah2 = uppol2(self.ah1, self.ah2, ph, self.ph1, self.ph2)

        # uppol1:  update first predictor coef. aph2 and delay it as ah1
        self.ah1 = uppol1(self.ah1, self.ah2, ph, self.ph1)

        # recons for higher sub-band
        yh = sh + dh

        # done with higher sub-band encoder, now Delay for next time
        self.rh2 = self.rh1
        self.rh1 = yh
        self.ph2 = self.ph1
        self.ph1 = ph

        # multiplex ih and il to get signals together
        return il | (ih << 6)


class ADPCM_Decoder:
    def __init__(self):
        self.dec_deth = 8
        self.dec_detl = 32

        self.dec_del_dhx = [0] * 6
        self.dec_del_bph = [0] * 6
        self.dec_del_bpl = [0] * 6
        self.dec_del_dltx = [0] * 6

        # variables for receive quadrature mirror filter here
        self.accumc = [0] * 11
        self.accumd = [0] * 11

        self.dec_plt1 = 0
        self.dec_plt2 = 0
        self.dec_rlt1 = 0
        self.dec_rlt2 = 0
        self.dec_al1 = 0
        self.dec_al2 = 0
        self.dec_nbl = 0
        self.dec_nbh = 0

        # variables used in filtep
        self.dec_rh1 = 0
        self.dec_rh2 = 0
        self.dec_ah1 = 0
        self.dec_ah2 = 0
        self.dec_ph1 = 0
        self.dec_ph2 = 0

    def decode(self, input):
        '''
        decode function, result in xout1 and xout2
        '''
        # split transmitted word from input into ilr and ih
        ilr = input & 0x3f
        ih = input >> 6

        # LOWER SUB_BAND DECODER

        # filtez: compute predictor output for zero section
        dec_szl = filtez(self.dec_del_bpl, self.dec_del_dltx)

        # filtep: compute predictor output signal for pole section
        #global dec_rlt1, dec_al1, dec_rlt2, dec_al2
        dec_spl = filtep(self.dec_rlt1, self.dec_al1, self.dec_rlt2, self.dec_al2)

        dec_sl = dec_spl + dec_szl

        # compute quantized difference signal for adaptive predic
        #global dec_detl
        dec_dlt = (self.dec_detl * qq4_code4_table[ilr >> 2]) >> 15

        # compute quantized difference signal for decoder output
        dl = (self.dec_detl * qq6_code6_table[ilr]) >> 15

        rl = dl + dec_sl
        
        # logscl: quantizer scale factor adaptation in the lower sub-band
        #global dec_nbl
        self.dec_nbl = logscl(ilr, self.dec_nbl)

        # scalel: computes quantizer scale factor in the lower sub band
        self.dec_detl = scalel(self.dec_nbl, 8)

        # parrec - add pole predictor output to quantized diff. signal
        # for partially reconstructed signal
        dec_plt = dec_dlt + dec_szl

        # upzero: update zero section predictor coefficients
        upzero(dec_dlt, self.dec_del_dltx, self.dec_del_bpl)

        # uppol2: update second predictor coefficient apl2 and delay it as al2
        #global dec_plt1, dec_plt2
        self.dec_al2 = uppol2(self.dec_al1, self.dec_al2, dec_plt, self.dec_plt1, self.dec_plt2)

        # uppol1: update first predictor coef. (pole setion)
        self.dec_al1 = uppol1(self.dec_al1, self.dec_al2, dec_plt, self.dec_plt1)
        
        # recons : compute recontructed signal for adaptive predictor
        dec_rlt = dec_sl + dec_dlt

        # done with lower sub band decoder, implement delays for next time
        self.dec_rlt2 = self.dec_rlt1
        self.dec_rlt1 = dec_rlt
        self.dec_plt2 = self.dec_plt1
        self.dec_plt1 = dec_plt

        # HIGH SUB-BAND DECODER

        # filtez: compute predictor output for zero section
        dec_szh = filtez(self.dec_del_bph, self.dec_del_dhx)

        # filtep: compute predictor output signal for pole section
        #global dec_rh1, dec_rh2, dec_ah1, dec_ah2
        dec_sph = filtep(self.dec_rh1, self.dec_ah1, self.dec_rh2, self.dec_ah2)

        # predic:compute the predictor output value in the higher sub_band decoder
        dec_sh = dec_sph + dec_szh

        # in-place compute the quantized difference signal
        #global dec_deth
        dec_dh = (self.dec_deth * qq2_code2_table[ih]) >> 15

        # logsch: update logarithmic quantizer scale factor in hi sub band
        #global dec_nbh
        self.dec_nbh = logsch(ih, self.dec_nbh)

        # scalel: compute the quantizer scale factor in the higher sub band
        self.dec_deth = scalel(self.dec_nbh, 10)

        # parrec: compute partially recontructed signal
        dec_ph = dec_dh + dec_szh

        # upzero: update zero section predictor coefficients
        upzero(dec_dh, self.dec_del_dhx, self.dec_del_bph)

        # uppol2: update second predictor coefficient aph2 and delay it as ah2
        #global dec_ph1, dec_ph2
        self.dec_ah2 = uppol2(self.dec_ah1, self.dec_ah2, dec_ph, self.dec_ph1, self.dec_ph2)

        # uppol1: update first predictor coef. (pole setion)
        self.dec_ah1 = uppol1(self.dec_ah1, self.dec_ah2, dec_ph, self.dec_ph1)

        # recons : compute recontructed signal for adaptive predictor
        rh = dec_sh + dec_dh

        # done with high band decode, implementing delays for next time here
        self.dec_rh2 = self.dec_rh1
        self.dec_rh1 = rh
        self.dec_ph2 = self.dec_ph1
        self.dec_ph1 = dec_ph

        # end of higher sub_band decoder

        # end with receive quadrature mirror filters
        xd = rl - rh
        xs = rl + rh
        
        # receive quadrature mirror filters implemented here
        xa1 = xd * COEFFS[0]
        xa2 = xs * COEFFS[1]

        # main multiply accumulate loop for samples and coefficients
        for i in range(10):
            xa1 += self.accumc[i] * COEFFS[i * 2 + 2]
            xa2 += self.accumd[i] * COEFFS[i * 2 + 3]
        # final mult/accumulate
        xa1 += self.accumc[10] * COEFFS[22]
        xa2 += self.accumd[10] * COEFFS[23]

        # scale by 2^14
        xout1 = xa1 >> 14
        xout2 = xa2 >> 14

        #print('xout1', xout1)
        # update delay lines
        for i in range(10, 0, -1):
            self.accumc[i] = self.accumc[i - 1]
            self.accumd[i] = self.accumd[i - 1]        
        
        self.accumc[0] = xd
        self.accumd[0] = xs

        return xout1, xout2

    def dump(self):
        print('dec_detl', self.dec_detl)
        print('dec_deth', self.dec_deth)
        print('dec_nbl', self.dec_nbl)
        print('dec_al1', self.dec_al1)
        print('dec_al2', self.dec_al2)
        print('dec_plt1', self.dec_plt1)
        print('dec_plt2', self.dec_plt2)
        print('dec_rlt1', self.dec_rlt1)
        print('dec_rlt2', self.dec_rlt2)
        print('dec_nbh', self.dec_nbh)
        print('dec_ah1', self.dec_ah1)
        print('dec_ah2', self.dec_ah2)
        print('dec_ph1', self.dec_ph1)
        print('dec_ph2', self.dec_ph2)
        print('dec_rh1', self.dec_rh1)
        print('dec_rh2', self.dec_rh2)
        
        #self.dec_del_dhx = [0] * 6
        #self.dec_del_bph = [0] * 6
        #self.dec_del_bpl = [0] * 6
        #self.dec_del_dltx = [0] * 6

        #self.accumc = [0] * 11
        #self.accumd = [0] * 11

        

SIZE = 100
IN_END = 100

test_data = (
    0x44, 0x44, 0x44, 0x44, 0x44,
    0x44, 0x44, 0x44, 0x44, 0x44,
    0x44, 0x44, 0x44, 0x44, 0x44,
    0x44, 0x44, 0x43, 0x43, 0x43,
    0x43, 0x43, 0x43, 0x43, 0x42,
    0x42, 0x42, 0x42, 0x42, 0x42,
    0x41, 0x41, 0x41, 0x41, 0x41,
    0x40, 0x40, 0x40, 0x40, 0x40,
    0x40, 0x40, 0x40, 0x3f, 0x3f,
    0x3f, 0x3f, 0x3f, 0x3e, 0x3e,
    0x3e, 0x3e, 0x3e, 0x3e, 0x3d,
    0x3d, 0x3d, 0x3d, 0x3d, 0x3d,
    0x3c, 0x3c, 0x3c, 0x3c, 0x3c,
    0x3c, 0x3c, 0x3c, 0x3c, 0x3b,
    0x3b, 0x3b, 0x3b, 0x3b, 0x3b,
    0x3b, 0x3b, 0x3b, 0x3b, 0x3b,
    0x3b, 0x3b, 0x3b, 0x3b, 0x3b,
    0x3b, 0x3b, 0x3b, 0x3b, 0x3b,
    0x3b, 0x3b, 0x3c, 0x3c, 0x3c,
    0x3c, 0x3c, 0x3c, 0x3c, 0x3c
)

test_compressed = (
    0xfd, 0xde, 0x77, 0xba, 0xf2, 
    0x90, 0x20, 0xa0, 0xec, 0xed, 
    0xef, 0xf1, 0xf3, 0xf4, 0xf5, 
    0xf5, 0xf5, 0xf5, 0xf6, 0xf6, 
    0xf6, 0xf7, 0xf8, 0xf7, 0xf8, 
    0xf7, 0xf9, 0xf8, 0xf7, 0xf9, 
    0xf8, 0xf8, 0xf6, 0xf8, 0xf8, 
    0xf7, 0xf9, 0xf9, 0xf9, 0xf8, 
    0xf7, 0xfa, 0xf8, 0xf8, 0xf7, 
    0xfb, 0xfa, 0xf9, 0xf8, 0xf8
)
test_result = (
    0, -1, -1, 0, 0, 
    -1, 0, 0, -1, -1, 
    0, 0, 0x1, 0x1, 0, 
    -2, -1, -2, 0, -4, 
    0x1, 0x1, 0x1, -5, 0x2, 
    0x2, 0x3, 0xb, 0x14, 0x14, 
    0x16, 0x18, 0x20, 0x21, 0x26, 
    0x27, 0x2e, 0x2f, 0x33, 0x32, 
    0x35, 0x33, 0x36, 0x34, 0x37, 
    0x34, 0x37, 0x35, 0x38, 0x36, 
    0x39, 0x38, 0x3b, 0x3a, 0x3f, 
    0x3f, 0x40, 0x3a, 0x3d, 0x3e, 
    0x41, 0x3c, 0x3e, 0x3f, 0x42, 
    0x3e, 0x3b, 0x37, 0x3b, 0x3e, 
    0x41, 0x3b, 0x3b, 0x3a, 0x3b, 
    0x36, 0x39, 0x3b, 0x3f, 0x3c, 
    0x3b, 0x37, 0x3b, 0x3d, 0x41, 
    0x3d, 0x3e, 0x3c, 0x3e, 0x3b, 
    0x3a, 0x37, 0x3b, 0x3e, 0x41, 
    0x3c, 0x3b, 0x39, 0x3a, 0x36
)


def adpcm_main(compressed, result):
    # reset, initialize required memory
    encoder = ADPCM_Encoder()
    decoder = ADPCM_Decoder()
    for i in range(0, IN_END, 2):
        compressed[i // 2] = encoder.encode(test_data[i], test_data[i + 1])
    for i in range(0, IN_END, 2):
        xout1, xout2 = decoder.decode(compressed[i // 2])
        result[i] = xout1
        result[i + 1] = xout2
    #decoder.dump()

@polyphony.testbench
def test():
    compressed = [0] * SIZE
    result = [0] * SIZE
    main_result = 0
    adpcm_main(compressed, result)
    for i in range(IN_END // 2):
        if compressed[i] != test_compressed[i]:
            main_result += 1
    print(main_result)
    for i in range(IN_END):
        #print('result', i, result[i])
        if result[i] != test_result[i]:
            main_result += 1
    print(main_result)
    #dump()
    #return main_result


test()
```
