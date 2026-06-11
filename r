using System;
using System.Text;

namespace MatrixCalculator
{
    public class MatrixException : Exception
    {
        public MatrixException(string message) : base(message) { }
    }

    public class MatrixSizeException : MatrixException
    {
        public MatrixSizeException(string message) : base(message) { }
    }

    public class MatrixSingularException : MatrixException
    {
        public MatrixSingularException(string message) : base(message) { }
    }

    public interface IPrototype<T>
    {
        T Clone();
    }

    public class SquareMatrix : IComparable<SquareMatrix>, IPrototype<SquareMatrix>
    {
        private double[,] data;

        public int Size { get; }

        public SquareMatrix(int size)
        {
            if (size <= 0)
                throw new MatrixSizeException("Размер матрицы должен быть больше нуля.");

            Size = size;
            data = new double[size, size];
        }

        public SquareMatrix(int size, int minValue, int maxValue) : this(size)
        {
            Random rnd = new Random();

            for (int i = 0; i < Size; i++)
                for (int j = 0; j < Size; j++)
                    data[i, j] = rnd.Next(minValue, maxValue + 1);
        }

        public SquareMatrix(double[,] array)
        {
            if (array.GetLength(0) != array.GetLength(1))
                throw new MatrixSizeException("Матрица должна быть квадратной.");

            Size = array.GetLength(0);
            data = new double[Size, Size];

            for (int i = 0; i < Size; i++)
                for (int j = 0; j < Size; j++)
                    data[i, j] = array[i, j];
        }

        public double this[int i, int j]
        {
            get => data[i, j];
            set => data[i, j] = value;
        }

        public static SquareMatrix operator +(SquareMatrix a, SquareMatrix b)
        {
            CheckSameSize(a, b);

            SquareMatrix result = new SquareMatrix(a.Size);

            for (int i = 0; i < a.Size; i++)
                for (int j = 0; j < a.Size; j++)
                    result[i, j] = a[i, j] + b[i, j];

            return result;
        }

        public static SquareMatrix operator *(SquareMatrix a, SquareMatrix b)
        {
            CheckSameSize(a, b);

            SquareMatrix result = new SquareMatrix(a.Size);

            for (int i = 0; i < a.Size; i++)
                for (int j = 0; j < a.Size; j++)
                    for (int k = 0; k < a.Size; k++)
                        result[i, j] += a[i, k] * b[k, j];

            return result;
        }

        public static bool operator >(SquareMatrix a, SquareMatrix b)
        {
            return a.Determinant() > b.Determinant();
        }

        public static bool operator <(SquareMatrix a, SquareMatrix b)
        {
            return a.Determinant() < b.Determinant();
        }

        public static bool operator >=(SquareMatrix a, SquareMatrix b)
        {
            return a.Determinant() >= b.Determinant();
        }

        public static bool operator <=(SquareMatrix a, SquareMatrix b)
        {
            return a.Determinant() <= b.Determinant();
        }

        public static bool operator ==(SquareMatrix a, SquareMatrix b)
        {
            if (ReferenceEquals(a, b))
                return true;

            if (a is null || b is null)
                return false;

            return a.Equals(b);
        }

        public static bool operator !=(SquareMatrix a, SquareMatrix b)
        {
            return !(a == b);
        }

        public static bool operator true(SquareMatrix matrix)
        {
            return Math.Abs(matrix.Determinant()) > 0.000001;
        }

        public static bool operator false(SquareMatrix matrix)
        {
            return Math.Abs(matrix.Determinant()) <= 0.000001;
        }

        public static explicit operator double(SquareMatrix matrix)
        {
            return matrix.Determinant();
        }

        public static implicit operator SquareMatrix(double[,] array)
        {
            return new SquareMatrix(array);
        }

        public double Determinant()
        {
            double[,] temp = (double[,])data.Clone();
            double det = 1;

            for (int i = 0; i < Size; i++)
            {
                int pivot = i;

                for (int j = i + 1; j < Size; j++)
                    if (Math.Abs(temp[j, i]) > Math.Abs(temp[pivot, i]))
                        pivot = j;

                if (Math.Abs(temp[pivot, i]) < 0.000001)
                    return 0;

                if (pivot != i)
                {
                    SwapRows(temp, i, pivot);
                    det *= -1;
                }

                det *= temp[i, i];

                for (int j = i + 1; j < Size; j++)
                {
                    double factor = temp[j, i] / temp[i, i];

                    for (int k = i; k < Size; k++)
                        temp[j, k] -= factor * temp[i, k];
                }
            }

            return det;
        }

        public SquareMatrix Inverse()
        {
            double det = Determinant();

            if (Math.Abs(det) < 0.000001)
                throw new MatrixSingularException("Обратная матрица не существует, так как детерминант равен нулю.");

            int n = Size;
            double[,] a = (double[,])data.Clone();
            double[,] inverse = new double[n, n];

            for (int i = 0; i < n; i++)
                inverse[i, i] = 1;

            for (int i = 0; i < n; i++)
            {
                double pivot = a[i, i];

                if (Math.Abs(pivot) < 0.000001)
                    throw new MatrixSingularException("Невозможно найти обратную матрицу.");

                for (int j = 0; j < n; j++)
                {
                    a[i, j] /= pivot;
                    inverse[i, j] /= pivot;
                }

                for (int row = 0; row < n; row++)
                {
                    if (row == i)
                        continue;

                    double factor = a[row, i];

                    for (int col = 0; col < n; col++)
                    {
                        a[row, col] -= factor * a[i, col];
                        inverse[row, col] -= factor * inverse[i, col];
                    }
                }
            }

            return new SquareMatrix(inverse);
        }

        public SquareMatrix Clone()
        {
            return new SquareMatrix(data);
        }

        public int CompareTo(SquareMatrix other)
        {
            if (other == null)
                return 1;

            return Determinant().CompareTo(other.Determinant());
        }

        public override bool Equals(object obj)
        {
            if (obj is not SquareMatrix other)
                return false;

            if (Size != other.Size)
                return false;

            for (int i = 0; i < Size; i++)
                for (int j = 0; j < Size; j++)
                    if (Math.Abs(data[i, j] - other[i, j]) > 0.000001)
                        return false;

            return true;
        }

        public override int GetHashCode()
        {
            int hash = Size;

            for (int i = 0; i < Size; i++)
                for (int j = 0; j < Size; j++)
                    hash = hash * 31 + data[i, j].GetHashCode();

            return hash;
        }

        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();

            for (int i = 0; i < Size; i++)
            {
                for (int j = 0; j < Size; j++)
                    sb.Append($"{data[i, j],8:F2}");

                sb.AppendLine();
            }

            return sb.ToString();
        }

        private static void CheckSameSize(SquareMatrix a, SquareMatrix b)
        {
            if (a.Size != b.Size)
                throw new MatrixSizeException("Размеры матриц должны совпадать.");
        }

        private static void SwapRows(double[,] array, int row1, int row2)
        {
            int n = array.GetLength(0);

            for (int i = 0; i < n; i++)
            {
                double temp = array[row1, i];
                array[row1, i] = array[row2, i];
                array[row2, i] = temp;
            }
        }
    }

    class Program
    {
        static void Main()
        {
            try
            {
                Console.WriteLine("Матричный калькулятор");

                SquareMatrix matrixA = new SquareMatrix(3, 1, 5);
                SquareMatrix matrixB = new SquareMatrix(3, 1, 5);

                Console.WriteLine("\nМатрица A:");
                Console.WriteLine(matrixA);

                Console.WriteLine("Матрица B:");
                Console.WriteLine(matrixB);

                Console.WriteLine("A + B:");
                Console.WriteLine(matrixA + matrixB);

                Console.WriteLine("A * B:");
                Console.WriteLine(matrixA * matrixB);

                Console.WriteLine($"det(A) = {matrixA.Determinant():F2}");
                Console.WriteLine($"det(B) = {matrixB.Determinant():F2}");

                Console.WriteLine($"A > B: {matrixA > matrixB}");
                Console.WriteLine($"A < B: {matrixA < matrixB}");
                Console.WriteLine($"A == B: {matrixA == matrixB}");

                Console.WriteLine("\nПроверка приведения типа:");
                double detA = (double)matrixA;
                Console.WriteLine($"double detA = {detA:F2}");

                Console.WriteLine("\nПроверка true / false:");
                if (matrixA)
                    Console.WriteLine("Матрица A невырожденная.");
                else
                    Console.WriteLine("Матрица A вырожденная.");

                Console.WriteLine("\nОбратная матрица A:");
                Console.WriteLine(matrixA.Inverse());

                Console.WriteLine("Проверка паттерна Prototype:");
                SquareMatrix copy = matrixA.Clone();
                Console.WriteLine("Копия матрицы A:");
                Console.WriteLine(copy);

                Console.WriteLine($"Equals: {matrixA.Equals(copy)}");
                Console.WriteLine($"CompareTo: {matrixA.CompareTo(matrixB)}");
                Console.WriteLine($"GetHashCode A: {matrixA.GetHashCode()}");
            }
            catch (MatrixException ex)
            {
                Console.WriteLine("Ошибка матрицы: " + ex.Message);
            }
            catch (Exception ex)
            {
                Console.WriteLine("Общая ошибка: " + ex.Message);
            }
        }
    }
}
