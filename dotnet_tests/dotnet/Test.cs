using System;
using System.Collections.Generic;
using Xunit;
using rust_swig_test_dotnet;

namespace dotnet
{
    public class UnitTest1
    {
        [Fact]
        public void Test1()
        {
            TestStaticClass.Hello();
            TestStaticClass.PrintNumber(123);
            Console.Out.WriteLine(TestStaticClass.Add(1, 2));
            Console.Out.WriteLine(TestStaticClass.Concat("Concatenated ", "String"));
            Console.Out.WriteLine(TestStaticClass.ConcatStr("Concatenated ", "str"));


            var obj = new TestClass();
            obj.Print();
            obj.Increment();
            obj.Print();
            obj.Add(3);
            obj.Print();
            Console.Out.WriteLine(obj.Get());
            
            TestStaticClass.TestObjByValue(obj);

            var vec = new List<int>();
            vec.Add(1);
            vec.Add(2);

            TestStaticClass.PrintVecLen(vec);
            var new_vec = TestStaticClass.GetVec();
            foreach (var e in new_vec)
            {
                Console.Out.WriteLine(e);
            }

            TestStaticClass.MaybeReturnClass(new Option<string>("asdf")).Value.Print();
            Console.Out.WriteLine(TestStaticClass.MaybeAddOne(new Option<int>()).IsSome);

            TestStaticClass.TryCreateObjectOk().Print();
            try 
            {
                TestStaticClass.TryCreateObjectErr();
            } catch (Error err)
            {
                Console.Out.WriteLine(err);
            }

            var arc_mutex = new TestArcMutex();
            arc_mutex.Inc();
            Console.Out.WriteLine(arc_mutex.ToString());
            Console.Out.WriteLine(TestArcMutex.ToStringArc(arc_mutex));

            Console.Out.WriteLine(TestStaticClass.ReverseEnum(TestEnum.B));

            var tuple = TestStaticClass.GetTuple();
            Console.Out.WriteLine(tuple.Item1);
            Console.Out.WriteLine(tuple.Item2);
        }
    }
}

