use {
    serde::{de::DeserializeOwned, ser, Deserialize, Deserializer, Serialize, Serializer},
    thiserror::Error,
};

#[cfg(feature = "derive")]
pub use serde_owned_derive::SerdeOwned;

use std::fmt;

pub use serde;

/// A serialization error.
///
/// Returns ownership of the `T`.
#[derive(Error)]
#[error("serializetion error: {}", error)]
pub struct SerIntoError<T, E>
where
    E: fmt::Display + fmt::Debug,
{
    inner: T,
    error: E,
}

impl<T, E> SerIntoError<T, E>
where
    E: fmt::Display + fmt::Debug,
{
    pub fn new(inner: T, error: E) -> Self {
        Self { error, inner }
    }

    pub fn into_parts(self) -> (T, E) {
        (self.inner, self.error)
    }

    pub fn map_err<U, F>(self, f: F) -> SerIntoError<T, U>
    where
        F: FnOnce(E) -> U,
        U: ser::Error,
    {
        SerIntoError {
            inner: self.inner,
            error: f(self.error),
        }
    }
}

impl<T, E> fmt::Debug for SerIntoError<T, E>
where
    E: fmt::Display + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("SerIntoError<T,E>")
            .field("inner", &"...")
            .field("error", &self.error)
            .finish()
    }
}

/// A value that can be serialized and deserialized while maintaining
/// ownership.
///
/// To do so, it converts to-and-fro a `Proxy` type which implements
/// `Serialize` and `DeserializeOwned`.
pub trait SerdeOwned {
    type Proxy: Serialize + DeserializeOwned;

    /// The caller must ensure that the `Proxy` was not duplicated so
    /// that ownership guarantees are maintained.
    unsafe fn from_proxy(proxy: Self::Proxy) -> Self;

    /// The caller must ensure that the `Proxy` will not be duplicated
    /// as to maintain ownership guarantees.
    unsafe fn into_proxy(self) -> Self::Proxy;
}

impl<T> SerdeOwned for T
where
    T: Serialize + DeserializeOwned,
{
    type Proxy = T;

    unsafe fn from_proxy(proxy: T) -> Self {
        proxy
    }

    unsafe fn into_proxy(self) -> T {
        self
    }
}

/// Serialization that transfers ownership.
pub trait SerializeInto: Sized {
    fn serialize_into<S>(self, serializer: S) -> Result<S::Ok, SerIntoError<Self, S::Error>>
    where
        S: Serializer;
}

/// Deserialization that is safe in case of exclusive ownership.
pub trait DeserializeFrom: Sized {
    unsafe fn deserialize_from<'de, D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>;
}

/// Serialization that transfers ownership.
impl<T> SerializeInto for T
where
    T: SerdeOwned,
{
    fn serialize_into<S>(self, serializer: S) -> Result<S::Ok, SerIntoError<Self, S::Error>>
    where
        S: Serializer,
    {
        // This is safe because we make sure that the proxy is not duplicated
        // or leaked.
        let proxy = unsafe { self.into_proxy() };
        proxy.serialize(serializer).map_err(|err| {
            let this = unsafe { T::from_proxy(proxy) };
            SerIntoError::new(this, err)
        })
    }
}

/// Deserialization that assumes exclusive ownership of the serialized bytes.
impl<T> DeserializeFrom for T
where
    T: SerdeOwned,
{
    /// The caller must make sure that he is the exclusive owner of the
    /// serialized bytes. He must also ensure that these bytes are not leaked
    /// upon success, so that the `T` can't be duplicated.
    unsafe fn deserialize_from<'de, D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let proxy = T::Proxy::deserialize(deserializer)?;
        Ok(T::from_proxy(proxy))
    }
}
