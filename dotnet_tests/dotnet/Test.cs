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
            TestStaticClass.hello();
            TestStaticClass.print_number(123);
            Console.Out.WriteLine(TestStaticClass.add(1, 2));
            Console.Out.WriteLine(TestStaticClass.concat("Concatenated ", "String"));

            var obj = new TestClass();
            obj.print();
            obj.increment();
            obj.print();
            obj.add(3);
            obj.print();

            Console.Out.WriteLine(obj.get());
        }
    }
}
