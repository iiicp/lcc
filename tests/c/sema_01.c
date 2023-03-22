int main() {
  /// error
  /// extern meaning static lifetime, external linkage
  /// block scope variable meaning auto lifetime, none linkage
  /// 没办法把i变量给外部使用.
  extern int i = 0;
  /// why no error?
  /// static meaning static lifetime, internal linkage
  /// block scope variable meaning auto lifetime, none linkage
  static int j = 0;
  return 0;
}