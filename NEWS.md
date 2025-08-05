## corrselect 1.0.1

### Improvements

- `corrSelect()` and `MatSelect()` now **automatically select the best algorithm** based on `force_in`:
  - Defaults to **ELS** when `force_in` is specified
  - Defaults to **Bron–Kerbosch** otherwise  
- Optimized `runELS()`:
  - Skips seed expansion when `force_in` is non-empty, improving performance up to **5×**
  - Precomputes a binary compatibility matrix to speed up correlation checks
  - Explicitly filters seeds to those compatible with `force_in`, reducing overhead
- Reduced duplicate work and memory use in ELS by hashing only canonical combinations

### Result

- **ELS now outperforms BK** in constrained cases with forced-in variables  
- Full backward compatibility with `method = "els"` or `"bron-kerbosch"`
