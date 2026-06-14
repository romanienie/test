using System;
using System.Text;

class MatrixException : Exception{

    public MatrixException(string message) : base(message) { }
}

interface IPrototype<T>{

    T Clone();
}

class SquareMatrix : IComparable<SquareMatrix>, IPrototype<SquareMatrix>{

    private readonly double[,] values;

    public int Size { get; }

    public SquareMatrix(int size){

        if (size <= 0)
            throw new MatrixException("Размер матрицы должен быть больше нуля.");

        Size = size;
        values = new double[size, size];
    }

    public SquareMatrix(int size, int minValue, int maxValue) : this(size){

        Random random = new Random();

        for (int row = 0; row < Size; row++){

            for (int column = 0; column < Size; column++){

                values[row, column] = random.Next(minValue, maxValue + 1);
            }
        }
    }

    public SquareMatrix(double[,] sourceValues){

        if (sourceValues.GetLength(0) != sourceValues.GetLength(1))
            throw new MatrixException("Матрица должна быть квадратной.");

        Size = sourceValues.GetLength(0);
        values = new double[Size, Size];

        for (int row = 0; row < Size; row++){

            for (int column = 0; column < Size; column++){

                values[row, column] = sourceValues[row, column];
            }
        }
    }

    public double this[int row, int column]{

        get{

            CheckIndex(row, column);
            return values[row, column];
        }
        set{

            CheckIndex(row, column);
            values[row, column] = value;
        }
    }

    private void CheckIndex(int row, int column){

        if (row < 0 || row >= Size || column < 0 || column >= Size)
            throw new MatrixException("Индекс выходит за границы матрицы.");
    }

    private static void CheckSameSize(SquareMatrix firstMatrix, SquareMatrix secondMatrix){

        if (firstMatrix.Size != secondMatrix.Size)
            throw new MatrixException("Размеры матриц должны совпадать.");
    }

    public static SquareMatrix operator +(SquareMatrix firstMatrix, SquareMatrix secondMatrix){

        CheckSameSize(firstMatrix, secondMatrix);

        SquareMatrix resultMatrix = new SquareMatrix(firstMatrix.Size);

        for (int row = 0; row < firstMatrix.Size; row++){

            for (int column = 0; column < firstMatrix.Size; column++){

                resultMatrix[row, column] = firstMatrix[row, column] + secondMatrix[row, column];
            }
        }

        return resultMatrix;
    }

    public static SquareMatrix operator *(SquareMatrix firstMatrix, SquareMatrix secondMatrix){

        CheckSameSize(firstMatrix, secondMatrix);

        SquareMatrix resultMatrix = new SquareMatrix(firstMatrix.Size);

        for (int row = 0; row < firstMatrix.Size; row++){

            for (int column = 0; column < firstMatrix.Size; column++){

                double sum = 0;

                for (int index = 0; index < firstMatrix.Size; index++){

                    sum += firstMatrix[row, index] * secondMatrix[index, column];
                }

                resultMatrix[row, column] = sum;
            }
        }

        return resultMatrix;
    }

    public double Determinant(){

        double[,] matrixCopy = (double[,])values.Clone();
        double determinant = 1;

        for (int pivot = 0; pivot < Size; pivot++){

            int pivotRow = pivot;

            for (int row = pivot + 1; row < Size; row++){

                if (Math.Abs(matrixCopy[row, pivot]) > Math.Abs(matrixCopy[pivotRow, pivot]))
                    pivotRow = row;
            }

            if (Math.Abs(matrixCopy[pivotRow, pivot]) < 0.000001)
                return 0;

            if (pivotRow != pivot){

                SwapRows(matrixCopy, pivotRow, pivot);
                determinant *= -1;
            }

            determinant *= matrixCopy[pivot, pivot];

            for (int row = pivot + 1; row < Size; row++){

                double factor = matrixCopy[row, pivot] / matrixCopy[pivot, pivot];

                for (int column = pivot; column < Size; column++){

                    matrixCopy[row, column] -= factor * matrixCopy[pivot, column];
                }
            }
        }

        return determinant;
    }

    public SquareMatrix Inverse(){

        double determinant = Determinant();

        if (Math.Abs(determinant) < 0.000001)
            throw new MatrixException("Обратная матрица не существует.");

        double[,] leftMatrix = (double[,])values.Clone();
        double[,] rightMatrix = new double[Size, Size];

        for (int i = 0; i < Size; i++)
            rightMatrix[i, i] = 1;

        for (int pivot = 0; pivot < Size; pivot++){

            double pivotValue = leftMatrix[pivot, pivot];

            if (Math.Abs(pivotValue) < 0.000001){

                for (int row = pivot + 1; row < Size; row++){

                    if (Math.Abs(leftMatrix[row, pivot]) > 0.000001){

                        SwapRows(leftMatrix, pivot, row);
                        SwapRows(rightMatrix, pivot, row);
                        break;
                    }
                }

                pivotValue = leftMatrix[pivot, pivot];
            }

            for (int column = 0; column < Size; column++){

                leftMatrix[pivot, column] /= pivotValue;
                rightMatrix[pivot, column] /= pivotValue;
            }

            for (int row = 0; row < Size; row++){

                if (row == pivot)
                    continue;

                double factor = leftMatrix[row, pivot];

                for (int column = 0; column < Size; column++){

                    leftMatrix[row, column] -= factor * leftMatrix[pivot, column];
                    rightMatrix[row, column] -= factor * rightMatrix[pivot, column];
                }
            }
        }

        return new SquareMatrix(rightMatrix);
    }

    private static void SwapRows(double[,] matrix, int firstRow, int secondRow){

        int size = matrix.GetLength(0);

        for (int column = 0; column < size; column++){

            double temp = matrix[firstRow, column];
            matrix[firstRow, column] = matrix[secondRow, column];
            matrix[secondRow, column] = temp;
        }
    }

    public int CompareTo(SquareMatrix? otherMatrix){

        if (otherMatrix == null)
            return 1;

        return Determinant().CompareTo(otherMatrix.Determinant());
    }

    public override bool Equals(object? obj){

        if (obj is not SquareMatrix otherMatrix)
            return false;

        if (Size != otherMatrix.Size)
            return false;

        for (int row = 0; row < Size; row++){

            for (int column = 0; column < Size; column++){

                if (Math.Abs(values[row, column] - otherMatrix[row, column]) > 0.000001)
                    return false;
            }
        }

        return true;
    }

    public override int GetHashCode(){

        int hashCode = Size;

        foreach (double value in values)
            hashCode = hashCode * 31 + value.GetHashCode();

        return hashCode;
    }

    public override string ToString(){

        StringBuilder text = new StringBuilder();

        for (int row = 0; row < Size; row++){

            for (int column = 0; column < Size; column++){

                text.Append($"{values[row, column],8:F2}");
            }

            text.AppendLine();
        }

        return text.ToString();
    }

    public SquareMatrix Clone(){

        return new SquareMatrix(values);
    }

    public static explicit operator double[,](SquareMatrix matrix){

        return (double[,])matrix.values.Clone();
    }

    public static implicit operator SquareMatrix(double[,] values){

        return new SquareMatrix(values);
    }

    public static bool operator >(SquareMatrix firstMatrix, SquareMatrix secondMatrix){

        return firstMatrix.CompareTo(secondMatrix) > 0;
    }

    public static bool operator <(SquareMatrix firstMatrix, SquareMatrix secondMatrix){

        return firstMatrix.CompareTo(secondMatrix) < 0;
    }

    public static bool operator >=(SquareMatrix firstMatrix, SquareMatrix secondMatrix){

        return firstMatrix.CompareTo(secondMatrix) >= 0;
    }

    public static bool operator <=(SquareMatrix firstMatrix, SquareMatrix secondMatrix){

        return firstMatrix.CompareTo(secondMatrix) <= 0;
    }

    public static bool operator ==(SquareMatrix? firstMatrix, SquareMatrix? secondMatrix){

        if (ReferenceEquals(firstMatrix, secondMatrix))
            return true;

        if (firstMatrix is null || secondMatrix is null)
            return false;

        return firstMatrix.Equals(secondMatrix);
    }

    public static bool operator !=(SquareMatrix? firstMatrix, SquareMatrix? secondMatrix){

        return !(firstMatrix == secondMatrix);
    }

    public static bool operator true(SquareMatrix matrix){

        return Math.Abs(matrix.Determinant()) > 0.000001;
    }

    public static bool operator false(SquareMatrix matrix){

        return Math.Abs(matrix.Determinant()) < 0.000001;
    }
}

class Program{

    static void Main(){

        try{

            SquareMatrix firstMatrix = new SquareMatrix(3, 1, 5);
            SquareMatrix secondMatrix = new SquareMatrix(3, 1, 5);

            Console.WriteLine("Первая матрица:");
            Console.WriteLine(firstMatrix);

            Console.WriteLine("Вторая матрица:");
            Console.WriteLine(secondMatrix);

            Console.WriteLine("Сумма матриц:");
            Console.WriteLine(firstMatrix + secondMatrix);

            Console.WriteLine("Произведение матриц:");
            Console.WriteLine(firstMatrix * secondMatrix);

            Console.WriteLine($"Детерминант первой матрицы: {firstMatrix.Determinant():F2}");
            Console.WriteLine($"Детерминант второй матрицы: {secondMatrix.Determinant():F2}");

            Console.WriteLine($"firstMatrix > secondMatrix: {firstMatrix > secondMatrix}");
            Console.WriteLine($"firstMatrix == secondMatrix: {firstMatrix == secondMatrix}");

            if (firstMatrix)
                Console.WriteLine("Первая матрица имеет обратную матрицу.");
            else
                Console.WriteLine("Первая матрица не имеет обратной матрицы.");

            Console.WriteLine("Копия первой матрицы:");
            SquareMatrix copiedMatrix = firstMatrix.Clone();
            Console.WriteLine(copiedMatrix);

            Console.WriteLine("Обратная матрица:");
            Console.WriteLine(firstMatrix.Inverse());
        }
        catch (MatrixException exception){

            Console.WriteLine($"Ошибка: {exception.Message}");
        }
    }
}
