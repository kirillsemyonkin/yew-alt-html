//
// FilterResultOrDefault
//

pub trait FilterResultOrDefault<T, E: Default> {
    fn filter_or_default(self, filter: impl FnOnce(&T) -> bool) -> Self;
}

impl<T, E: Default> FilterResultOrDefault<T, E> for Result<T, E> {
    fn filter_or_default(self, filter: impl FnOnce(&T) -> bool) -> Self {
        match self {
            Ok(v) if filter(&v) => Ok(v),
            Ok(_) => Err(E::default()),
            Err(e) => Err(e),
        }
    }
}

//
// Append
//

pub trait ImplIntoExt<T> {
    fn append_from(&mut self, values: impl IntoIterator<Item = impl Into<T>>);

    fn push_from(&mut self, value: impl Into<T>);
}

impl<T> ImplIntoExt<T> for Vec<T> {
    fn append_from(&mut self, values: impl IntoIterator<Item = impl Into<T>>) {
        self.append(
            &mut values
                .into_iter()
                .map(Into::<T>::into)
                .collect(),
        )
    }

    fn push_from(&mut self, value: impl Into<T>) { self.push(value.into()) }
}

//
// MatchOpt
//

pub trait MatchOpt<T> {
    fn match_opt(self) -> Option<T>;
}