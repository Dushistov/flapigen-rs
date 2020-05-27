using System;
using Xunit;
using rust_swig_test_dotnet;

namespace dotnet
{
    public class UnitTest1
    {
        [Fact]
        public void Test1()
        {
            TestStaticClass.TestStaticClass_hello();
            TestStaticClass.TestStaticClass_print_number(123);
            Console.Out.WriteLine(TestStaticClass.TestStaticClass_add(1, 2));

            var obj = TestClass.TestClass_new();
            TestClass.TestClass_print(obj);
            TestClass.TestClass_increment(obj);
            TestClass.TestClass_print(obj);
            TestClass.TestClass_add(obj, 3);
            TestClass.TestClass_print(obj);
        }
    }
}
