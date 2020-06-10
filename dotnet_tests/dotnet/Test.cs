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

            var vec = new RustVecint();
            vec.Add(1);
            vec.Add(2);
            vec.Insert(0, 3);

            var new_vec = TestStaticClass.test_vec(vec);
            for (int i = 0; i < new_vec.Count; ++i)
            {
                Console.Out.WriteLine(++new_vec[i]);
            }
            new_vec.Dispose();

            TestStaticClass.maybe_return_class(new Option<string>("asdf")).Value.print();
            Console.Out.WriteLine(TestStaticClass.maybe_add_one(new Option<int>()).IsSome);
        }
    }
}
