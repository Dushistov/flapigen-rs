using System;
using System.Collections.Generic;
using Xunit;
using flapigen_test_dotnet;

namespace dotnet
{
    public class UnitTest
    {
        [Fact]
        /// "Smoke" tests for flapigen dotnet backend.
        /// Note, that some function does not return anything. They are called here just to verify, that there are no crashes.
        public void TestAll()
        {
            TestStaticClass.Hello();
            TestStaticClass.PrintNumber(123);
            Assert.Equal(3, TestStaticClass.Add(1, 2));
            Assert.Equal("Concatenated String", TestStaticClass.Concat("Concatenated ", "String"));
            Assert.Equal("Concatenated str", TestStaticClass.Concat("Concatenated ", "str"));

            var obj = new TestClass();
            Assert.Equal(0, obj.Get());
            obj.Increment();
            Assert.Equal(1, obj.Get());
            obj.Add(3);
            obj.Print();
            Assert.Equal(4, obj.Get());
            
            TestStaticClass.TestObjByValue(obj);

            var vec = new List<int>();
            vec.Add(1);
            vec.Add(2);

            TestStaticClass.PrintVecLen(vec);
            var new_vec = TestStaticClass.GetVec();
            Assert.Equal(3, new_vec.Count);
            Assert.Equal(5, new_vec[0]);
            Assert.Equal(6, new_vec[1]);
            Assert.Equal(7, new_vec[2]);

            Assert.Equal(0, TestStaticClass.MaybeReturnClass(new Option<string>("asdf")).Value.Get());
            Assert.False(TestStaticClass.MaybeAddOne(new Option<int>()).IsSome);

            // This shouldn't throw.
            TestStaticClass.TryCreateObjectOk();
            // But this one should.
            Assert.Throws<flapigen_test_dotnet.Error>(() => TestStaticClass.TryCreateObjectErr());
            
            var arc_mutex = new TestArcMutex();
            arc_mutex.Inc();
            Assert.Equal("1", arc_mutex.ToString());
            Assert.Equal("1", TestArcMutex.ToStringArc(arc_mutex));

            Assert.Equal(TestEnum.A, TestStaticClass.ReverseEnum(TestEnum.B));

            var tuple = TestStaticClass.GetTuple();
            Assert.Equal(0, tuple.Item1);
            Assert.Equal("0", tuple.Item2);
        }
    }
}

