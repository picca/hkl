// use is_matrix when possible

@@
identifier res, f;
expression E;
@@

(
res &= DIAG(E);
|
-res &= E;
+res &= DIAG(E);
)
