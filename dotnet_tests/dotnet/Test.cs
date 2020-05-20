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
            Console.Out.WriteLine("Hello from C#");
            TestStaticClass.hello();
        }
    }
}
