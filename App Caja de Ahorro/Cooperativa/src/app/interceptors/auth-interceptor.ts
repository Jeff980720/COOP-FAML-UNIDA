import { HttpInterceptorFn } from '@angular/common/http';

export const authInterceptor: HttpInterceptorFn = (req, next) => {
  // 1. Obtener el token del almacenamiento local
  const token = localStorage.getItem('token');

  // 2. Si el token existe, clonamos la petición y agregamos el header
  if (token) {
    const authReq = req.clone({
      setHeaders: {
        'x-token': token // Debe coincidir con req.header('x-token') de tu backend
      }
    });
    return next(authReq);
  }

  // 3. Si no hay token, la petición sigue su curso original (ej: para el login)
  return next(req);
};
