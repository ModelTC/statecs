pub(crate) mod component;
pub(crate) mod system;

pub mod prelude {
    pub use crate::component::{ComponentGet, ComponentPut};
    pub use crate::system::system;

    pub use crate::put;
    pub use crate::take;
}

pub use prelude::*;