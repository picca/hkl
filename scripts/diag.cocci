// use is_matrix when possible

@@
identifier res;
expression E;
@@

(
res &= DIAG(E);
|
-res &= E;
+res &= DIAG(E);
)
