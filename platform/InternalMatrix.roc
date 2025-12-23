module [RocMatrix, Matrix, from_matrix, to_matrix]
Matrix : {
    m0: F32,
    m4: F32,
    m8: F32,
    m12: F32,
    m1: F32,
    m5: F32,
    m9: F32,
    m13: F32,
    m2: F32,
    m6: F32,
    m10: F32,
    m14: F32,
    m3: F32,
    m7: F32,
    m11: F32,
    m15: F32,
}
RocMatrix := Matrix
from_matrix : {
    m0: F32,
    m4: F32,
    m8: F32,
    m12: F32,
    m1: F32,
    m5: F32,
    m9: F32,
    m13: F32,
    m2: F32,
    m6: F32,
    m10: F32,
    m14: F32,
    m3: F32,
    m7: F32,
    m11: F32,
    m15: F32,
} -> RocMatrix
from_matrix = |matrix|
    @RocMatrix(matrix)

to_matrix = |@RocMatrix(matrix)|
    matrix
