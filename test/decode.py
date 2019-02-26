```python:test.py
# +--------------------------------------------------------------------------+
    def decode(self, input):
        '''
        decode function, result in xout1 and xout2
        '''
        # split transmitted word from input into ilr and ih
        ilr = input & 0x3f
        ih = input >> 6
```
