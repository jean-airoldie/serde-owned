use serde_owned::{DeserializeFrom, SerdeOwned, SerializeInto};
use serde_owned_derive::SerdeOwned;

use std::fmt;

macro_rules! test_serde {
    ($ident:ident, $ty:ty) => {
        let vec = vec![];
        let mut ser = serde_json::Serializer::new(vec);
        $ident.clone().serialize_into(&mut ser).unwrap();

        let bytes = ser.into_inner();

        let mut de = serde_json::Deserializer::from_slice(&bytes);
        let _value = unsafe { <$ty>::deserialize_from(&mut de).unwrap() };
        assert_eq!($ident, _value);
    };
}

mod named_struct {
    use super::*;

    #[derive(SerdeOwned, Clone, PartialEq, Eq, Debug)]
    struct Struct {
        a: u32,
        b: String,
        c: Option<i32>,
    }

    #[test]
    fn serde_struct() {
        let value = Struct {
            a: 1,
            b: "ok".to_string(),
            c: None,
        };

        test_serde!(value, Struct);
    }

    #[derive(SerdeOwned, Clone, PartialEq, Eq, Debug)]
    struct Nested {
        named: Struct,
        a: u32,
    }

    #[test]
    fn serde_nested_struct() {
        let named = Struct {
            a: 1,
            b: "ok".to_string(),
            c: None,
        };
        let value = Nested { named, a: 2 };

        test_serde!(value, Nested);
    }

    #[derive(SerdeOwned, Clone, PartialEq, Eq, Debug)]
    struct Generic<A, B>
    where
        A: fmt::Debug + SerdeOwned + Clone + PartialEq + Eq,
        B: fmt::Debug + SerdeOwned + Clone + PartialEq + Eq,
    {
        a: u32,
        b: A,
        c: B,
    }

    #[test]
    fn serde_generic() {
        let value = Generic { a: 1, b: 1, c: 1 };

        test_serde!(value, Generic<i32, i32>);
    }

    #[derive(SerdeOwned, Clone, PartialEq, Eq, Debug)]
    struct GenericNested<A, B>
    where
        A: fmt::Debug + SerdeOwned + Clone + PartialEq + Eq,
        B: fmt::Debug + SerdeOwned + Clone + PartialEq + Eq,
    {
        a: Generic<A, B>,
    }

    #[test]
    fn serde_generic_nested() {
        let value = Generic { a: 1, b: 1, c: 1 };
        let value = GenericNested { a: value };

        test_serde!(value, GenericNested<i32, i32>);
    }
}

mod unnamed_struct {
    use super::*;
    #[derive(SerdeOwned, Clone, PartialEq, Eq, Debug)]
    struct GenericUnnamedStruct<A, B>(A, B)
    where
        A: fmt::Debug + SerdeOwned + Clone + PartialEq + Eq,
        B: fmt::Debug + SerdeOwned + Clone + PartialEq + Eq;

    #[test]
    fn serde_generic_unnamed_struct() {
        let value = GenericUnnamedStruct(1, 2);
        test_serde!(value, GenericUnnamedStruct<i32, i32>);
    }
    #[derive(SerdeOwned, Clone, PartialEq, Eq, Debug)]
    struct Struct(Option<Option<i32>>, i32);

    #[test]
    fn serde_struct() {
        let value = Struct(None, 0);
        test_serde!(value, Struct);
    }
}

mod enum_ {
    use super::*;

    #[derive(SerdeOwned, Clone, PartialEq, Eq, Debug)]
    enum Generic<A, B>
    where
        A: fmt::Debug + SerdeOwned + Clone + PartialEq + Eq,
        B: fmt::Debug + SerdeOwned + Clone + PartialEq + Eq,
    {
        A(A),
        B(B),
    }
    #[test]
    fn serde_generic() {
        let value = Generic::<i32, i32>::A(420);
        test_serde!(value, Generic<i32, i32>);
    }

    #[derive(SerdeOwned, Clone, Copy, PartialEq, Eq, Debug)]
    enum Enum {
        Unit,
        Unnamed(i32),
    }

    #[test]
    fn serde_enum() {
        let value = Enum::Unnamed(3);
        test_serde!(value, Enum);
    }

    #[derive(SerdeOwned, Clone, Copy, PartialEq, Eq, Debug)]
    enum Discriminant {
        One = 1,
        Two = 2,
    }

    #[test]
    fn serde_discriminant() {
        let value = Discriminant::One;
        test_serde!(value, Discriminant);
    }
}

mod krate {
    #[test]
    fn serde_crate() {
        use panini::{DeserializeFrom, SerializeInto};
        use serde_owned as panini;
        use serde_owned_derive::SerdeOwned;

        #[derive(SerdeOwned, Clone, PartialEq, Eq, Debug)]
        #[serde_owned(crate = "panini")]
        struct NamedStruct {
            a: u32,
            b: String,
            c: Option<i32>,
        }

        let value = NamedStruct {
            a: 1,
            b: "ok".to_string(),
            c: None,
        };
        test_serde!(value, NamedStruct);
    }
}
