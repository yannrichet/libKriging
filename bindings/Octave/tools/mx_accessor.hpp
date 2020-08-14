#ifndef LIBKRIGING_BINDINGS_OCTAVE_TOOLS_MX_ACCESSOR_HPP
#define LIBKRIGING_BINDINGS_OCTAVE_TOOLS_MX_ACCESSOR_HPP

#include <armadillo>
#include <cstring>

#include "ObjectAccessor.hpp"
#include "mex.h"

template <typename T>
struct converter_trait {
  using type = T;
};

template <>
struct converter_trait<ObjectRef> {
  using type = ObjectCollector::ref_t;
};

template <typename T>
auto converter(mxArray*);

template <typename T>
void setter(const T&, mxArray*&);

/* Specialization */

template <>
inline auto converter<mxArray*>(mxArray* x) {
  return x;
}

template <>
inline auto converter<std::string>(mxArray* x) {
  if (!mxIsChar(x) || mxGetNumberOfDimensions(x) != 2 || mxGetM(x) != 1 || mxGetM(x) != 1) {
    throw MxException(LOCATION(), "mLibKriging:badType", "not a string");
  }

  char buffer[256];
  if (mxGetString(x, buffer, 256) != 0) {
    throw MxException(LOCATION(), "mLibKriging:badType", "not a string");
  }

  return std::string{buffer};
}

template <>
inline auto converter<arma::vec>(mxArray* x) {
  if (!mxIsDouble(x) || mxIsComplex(x) || mxGetNumberOfDimensions(x) > 2) {
    throw MxException(LOCATION(), "mLibKriging:badType", "not a vector of double");
  }
  const arma::uword nrow = mxGetM(x);
  const arma::uword ncol = mxGetN(x);
  if (ncol > 1) {
    throw MxException(LOCATION(), "mLibKriging:badType", "not a vector of double");
  }
  double* data = mxGetPr(x);
  return arma::vec{data, nrow, false, true};
}

template <>
inline auto converter<arma::mat>(mxArray* x) {
  if (!mxIsDouble(x) || mxIsComplex(x) || mxGetNumberOfDimensions(x) > 2) {
    throw MxException(LOCATION(), "mLibKriging:badType", "not a matrix of double");
  }
  const arma::uword nrow = mxGetM(x);
  const arma::uword ncol = mxGetN(x);
  double* data = mxGetPr(x);
  return arma::mat{data, nrow, ncol, false, true};
}

template <>
inline auto converter<ObjectRef>(mxArray* x) {
  return getObject(x);
}

template <>
inline auto converter<uint64_t>(mxArray* x) {
  if (!mxIsUint64(x) || mxIsComplex(x) || mxGetNumberOfElements(x) != 1) {
    throw MxException(LOCATION(), "mLibKriging:badType", "not an unsigned 64bits int");
  }
  return *static_cast<uint64_t*>(mxGetData(x));
}

template <>
inline void setter<arma::vec>(const arma::vec& v, mxArray*& x) {
  x = mxCreateNumericMatrix(v.n_rows, v.n_cols, mxDOUBLE_CLASS, mxREAL);
  if (false && v.mem_state == 0 && v.n_elem > arma::arma_config::mat_prealloc) {
    // FIXME hard trick; use internal implementation of arma::~Mat
    arma::access::rw(v.mem_state) = 2;
    mxSetPr(x, const_cast<double*>(v.memptr()));
  } else {
    std::memcpy(mxGetPr(x), v.memptr(), sizeof(double) * v.n_rows * v.n_cols);
  }
}

template <>
inline void setter<uint64_t>(const uint64_t& v, mxArray*& x) {
  x = mxCreateNumericMatrix(1, 1, mxUINT64_CLASS, mxREAL);
  *static_cast<uint64_t*>(mxGetData(x)) = v;
}

template <>
inline void setter<EmptyObject>(const EmptyObject& v, mxArray*& x) {
  x = mxCreateNumericMatrix(0, 0, mxUINT64_CLASS, mxREAL);
}

#endif  // LIBKRIGING_BINDINGS_OCTAVE_TOOLS_MX_ACCESSOR_HPP
