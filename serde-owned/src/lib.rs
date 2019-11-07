use {
    serde::{Serializer, de::DeserializeOwned, Deserializer, Serialize, Deserialize, ser},
    thiserror::Error,
};

#[cfg(feature = "derive")]
pub use serde_owned_derive::SerdeOwned;

use std::fmt;

pub use serde;

#[derive(Error)]
#[error("serializetion error: {}", error)]
pub struct Error<T, E> where E: fmt::Display + fmt::Debug {
    inner: T,
    error: E,
}

impl<T, E> Error<T, E> where E: fmt::Display + fmt::Debug  {
    pub fn new(inner: T, error: E) -> Self {
        Self { error, inner }
    }

    pub fn into_parts(self) -> (T, E) {
        (self.inner, self.error)
    }

    pub fn map_err<U, F>(self, f: F) -> Error<T, U>
    where
        F: FnOnce(E) -> U,
        U: ser::Error,
    {
        Error {
            inner: self.inner,
            error: f(self.error),
        }
    }
}

impl<T, E> fmt::Debug for Error<T, E> where E: fmt::Display + fmt::Debug {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Error<T,E>")
            .field("inner", &"...")
            .field("error", &self.error)
            .finish()
    }
}

/// Convert a type that can be serialized & deserialized if ownership is
/// transfered.
///
/// To do so, it converts to-and-fro a `Proxy` type which implements
/// `Serialize` and `Deserialize`.
pub trait SerdeOwned {
    type Proxy: Serialize + DeserializeOwned;

    /// This is only safe if we are the exclusive owner of the `Proxy`.
    unsafe fn from_proxy(proxy: Self::Proxy) -> Self;

    fn into_proxy(self) -> Self::Proxy;
}

impl<T> SerdeOwned for T
where
    T: Serialize + DeserializeOwned,
{
    type Proxy = T;

    unsafe fn from_proxy(proxy: T) -> Self {
        proxy
    }

    fn into_proxy(self) -> T {
        self
    }
}

/// Serialization that transfers ownership.
pub trait SerializeInto: Sized {
    fn serialize_into<S>(self, serializer: S) -> Result<S::Ok, Error<Self, S::Error>>
    where
        S: Serializer;
}


/// Deserialization that is safe in case of exclusive ownership.
pub trait DeserializeFrom: Sized {
    unsafe fn deserialize_from<'de, D>(deserializer: D) -> Result<Self, D::Error> where
        D: Deserializer<'de>;
}

/// Serialization that transfers ownership.
impl<T> SerializeInto for T where T: SerdeOwned {
    fn serialize_into<S>(self, serializer: S) -> Result<S::Ok, Error<Self, S::Error>>
    where
        S: Serializer {
        let proxy = self.into_proxy();
        proxy.serialize(serializer).map_err(|err| {
            let this = unsafe { T::from_proxy(proxy) };
            Error::new(this, err)
        })
    }
}


/// Deserialization that is safe in case of exclusive ownership.
impl<T> DeserializeFrom for T where T: SerdeOwned {
    unsafe fn deserialize_from<'de, D>(deserializer: D) -> Result<Self, D::Error> where
        D: Deserializer<'de> {
            let proxy = T::Proxy::deserialize(deserializer)?;
            Ok(T::from_proxy(proxy))
    }
}
