import { Component } from '@angular/core';
import { Caja } from '../../models/Caja';
import { SociosService } from '../../services/socios';
import { Router } from '@angular/router';
import { CommonModule } from '@angular/common';
import { EstadoResultado } from '../estado-resultado/estado-resultado';
import Swal from 'sweetalert2';

@Component({
  selector: 'app-lista-caja-component',
  imports: [CommonModule],
  templateUrl: './lista-caja-component.html',
  styleUrl: './lista-caja-component.css',
  providers: [EstadoResultado] // Lo agregas como proveedor aquí
})
export class ListaCajaComponent {
  liquido: Caja[] = [];

  constructor(private service: SociosService, private router: Router, private estadoResultados: EstadoResultado) { }

  ngOnInit(): void {
    this.service.getCaja().subscribe(
      (data: any) => {
        // OPCIÓN A: Si quieres mantener la estructura de 'Coperativa'
        this.liquido = data; // Asignamos directamente el arreglo de aportes
        console.log('Datos mapeados:', this.liquido);
      },
      (error) => {
        console.error('Error:', error);
      }
    );
  }

  // En lista-aporte-component.ts

  // 1. ELIMINAR
  // eliminarCaja(idCaja: any): void {
  //   const password = prompt('Ingrese la contraseña de administrador:');

  //   if (password === '1998') {
  //     const confirmar = confirm(`¿Está seguro de eliminar el registro #${idCaja}?`);

  //     if (confirmar) {
  //       this.service.eliminarCaja(idCaja).subscribe({
  //         next: (res) => {
  //           // Solo si el servidor borra realmente, actualizamos la vista
  //           this.liquido = this.liquido.filter(c => c.idcaja !== idCaja);
  //           alert('Registro eliminado correctamente.');
  //         },
  //         error: (err) => {
  //           // Capturamos el mensaje de error del Backend
  //           if (err.status === 400 && err.error.message) {
  //             alert(err.error.message);
  //           } else {
  //             console.error('Error detallado:', err);
  //             alert('Error al intentar eliminar. Es posible que el registro tenga dependencias activas.');
  //           }
  //           // AL NO ESTAR EN 'NEXT', LA LISTA NO SE FILTRA Y EL REGISTRO PERMANECE VISIBLE
  //         }
  //       });
  //     }
  //   } else if (password !== null) {
  //     alert('Contraseña incorrecta.');
  //   }
  // }

  eliminarCaja(idCaja: any): void {
    Swal.fire({
      title: 'Seguridad de Administrador',
      input: 'password',
      inputLabel: 'Ingrese la contraseña para eliminar:',
      showCancelButton: true,
      confirmButtonText: 'Validar',
      cancelButtonText: 'Cancelar'
    }).then((result:any) => {
      if (result.value === '1998') {
        // Si la clave es correcta, procedemos
        this.ejecutarEliminacion(idCaja);
      } else if (result.isConfirmed) {
        Swal.fire('Error', 'Contraseña incorrecta', 'error');
      }
    });
  }

  // Mueve la lógica del subscribe a una función aparte
  private ejecutarEliminacion(idCaja: any) {
    this.service.eliminarCaja(idCaja).subscribe({
      next: () => {
        this.liquido = this.liquido.filter(a => a.idcaja !== idCaja);
        Swal.fire('Eliminado', 'Registro borrado con éxito', 'success');
      },
      error: (err) => {
        const msg = err.error?.message || 'No se puede eliminar porque tiene pagos.';
        Swal.fire('Error', msg, 'error');
      }
    });
  }

  // 2. ACTUALIZAR
  // editarCaja(caja: any) {

  //   const password = prompt('Ingrese la contraseña de administrador para editar el pago:');

  //   if (password === '1998') {
  //     console.log('Intentando editar el aporte con ID:', caja.idcaja);

  //     if (caja && caja.idcaja) {
  //       this.router.navigate(['/editar-caja', caja.idcaja])
  //         .then(nav => console.log('¿Navegación exitosa?:', nav))
  //         .catch(err => console.error('Error en navegación:', err));
  //     } else {
  //       console.error('El objeto aporte no tiene un idaporte válido', caja);
  //     }
  //   } else if (password !== null) {
  //     alert('Contraseña incorrecta.');
  //   }

  //   console.log('Intentando editar la caja con ID:', caja.idcaja);

  //   // if (caja && caja.idcaja) {
  //   //   this.router.navigate(['/editar-caja', caja.idcaja])
  //   //     .then(nav => console.log('¿Navegación exitosa?:', nav))
  //   //     .catch(err => console.error('Error en navegación:', err));
  //   // } else {
  //   //   console.error('El objeto caja no tiene un idcaja válido', caja);
  //   // }
  // }

  editarCaja(caja: any) {
    // 1. Sustitución de prompt por SweetAlert2 para evitar bloqueos en escritorio
    Swal.fire({
      title: 'Seguridad de Administrador',
      text: 'Ingrese la contraseña para editar el pago:',
      input: 'password',
      inputAttributes: {
        autocapitalize: 'off'
      },
      showCancelButton: true,
      confirmButtonText: 'Validar',
      cancelButtonText: 'Cancelar',
      confirmButtonColor: '#28a745',
      cancelButtonColor: '#d33'
    }).then((result:any) => {
      if (result.isConfirmed) {
        // 2. Validación de la clave '1998'
        if (result.value === '1998') {
          console.log('Intentando editar el caja con ID:', caja.idcaja);

          // 3. Verificación de ID y Navegación
          if (caja && caja.idcaja) {
            this.router.navigate(['/editar-caja', caja.idcaja])
              .then(nav => {
                if (nav) {
                  console.log('Navegación a edición de pagos exitosa');
                } else {
                  console.warn('La navegación falló. Revisa tus rutas.');
                }
              })
              .catch(err => console.error('Error en navegación:', err));
          } else {
            console.error('El objeto caja no tiene un idcaja válido', caja);
            Swal.fire('Error', 'No se encontró un ID de caja válido.', 'error');
          }
        } else {
          // Alerta de contraseña incorrecta
          Swal.fire({
            icon: 'error',
            title: 'Acceso Denegado',
            text: 'Contraseña incorrecta.',
            timer: 2000,
            showConfirmButton: false
          });
        }
      }
    });
  }

  irASocio(): void {
    this.router.navigate(['/socios']);
  }

  irFormCaja(): void {
    this.router.navigate(['/caja']);
  }

  botonDescargarPDF() {
    // Ahora 'listaPrestamos' ya existe y la función acepta 1 argumento
    this.estadoResultados.exportarListaCajaPDF();
  }
}
