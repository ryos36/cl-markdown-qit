```
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
```
