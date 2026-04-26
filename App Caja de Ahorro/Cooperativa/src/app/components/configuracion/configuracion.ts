import { Component, OnInit } from '@angular/core';
import { SociosService } from '../../services/socios';
import { RouterModule, RouterOutlet } from '@angular/router';
import { CommonModule } from '@angular/common';
import { MatIconModule } from '@angular/material/icon';
import { MatDialog } from '@angular/material/dialog';
import { UsuarioForm } from '../usuario-form/usuario-form';
import Swal from 'sweetalert2';



@Component({
  selector: 'app-configuracion',
  imports: [RouterModule, CommonModule, MatIconModule],
  templateUrl: './configuracion.html',
  styleUrl: './configuracion.css',
})
export class Configuracion implements OnInit {
  usuarios: any[] = [];

  constructor(private sociosService: SociosService, private dialog: MatDialog) { }

  ngOnInit() {
    this.cargarUsuarios();
  }


  cargarUsuarios() {
    this.sociosService.getUsuarios().subscribe({
      next: (data) => {
        this.usuarios = data;
      },
      error: (err) => {
        console.error('Falló la carga de usuarios', err);
      }
    });
  }

  // 1. Función auxiliar para reutilizar la validación
  private async validarAccion(): Promise<string | null> {
    const { value: password } = await Swal.fire({
      title: 'Validar Identidad',
      text: 'Se requiere contraseña de administrador para realizar cambios',
      input: 'password',
      inputPlaceholder: 'Ingresa tu contraseña',
      showCancelButton: true,
      confirmButtonColor: '#3085d6',
      cancelButtonColor: '#d33',
      confirmButtonText: 'Confirmar',
      cancelButtonText: 'Cancelar',
      background: '#1a1d21',
      color: '#ffffff',
      preConfirm: (value) => {
        if (!value) {
          Swal.showValidationMessage('La contraseña es obligatoria');
        }
        return value;
      }
    });
    return password || null;
  }

  // 2. Método Agregar
  async agregarUsuario() {
    const adminPass = await this.validarAccion();
    if (!adminPass) return;

    const dialogRef = this.dialog.open(UsuarioForm, {
      width: '450px',
      disableClose: true
    });

    dialogRef.afterClosed().subscribe(formData => {
      if (formData) {
        // Unificamos datos del form + contraseña admin
        const dataFinal = { ...formData, contrasenia: adminPass };

        this.sociosService.crearUsuario(dataFinal).subscribe({
          next: () => {
            this.cargarUsuarios();
            this.notificarExito('Usuario creado con éxito');
          },
          error: (err) => Swal.fire('Error', err.error.msg || 'No se pudo crear', 'error')
        });
      }
    });
  }

  // 3. Método Editar
  async editarUsuario(usuario: any) {
    const adminPass = await this.validarAccion();
    if (!adminPass) return;

    const dialogRef = this.dialog.open(UsuarioForm, {
      width: '450px',
      data: usuario // Pasamos el usuario actual al formulario
    });

    dialogRef.afterClosed().subscribe(formData => {
      if (formData) {
        // Unificamos cambios + contraseña admin
        const dataFinal = { ...formData, contrasenia: adminPass };

        // IMPORTANTE: Asegúrate de usar el campo ID correcto (idusuario)
        this.sociosService.actualizarUsuario(usuario.idusuario, dataFinal).subscribe({
          next: () => {
            this.cargarUsuarios();
            this.notificarExito('Usuario actualizado con éxito');
          },
          error: (err) => Swal.fire('Error', err.error.msg || 'No se pudo actualizar', 'error')
        });
      }
    });
  }

  // ELIMINA LA FU
  // En configuracion.ts
  async eliminarUsuario(id: number) {
    const contrasenia = await this.validarAccion(); // Pide la clave del admin
    if (!contrasenia) return;

    this.sociosService.eliminarUsuario(id, contrasenia).subscribe({
      next: () => {
        this.cargarUsuarios();
        this.notificarExito('Usuario eliminado');
      },
      error: (err) => {
        Swal.fire('Error', err.error.msg || 'No se pudo eliminar', 'error');
      }
    });
  }

  // Función extra para mensajes de éxito
  private notificarExito(mensaje: string) {
    Swal.fire({
      icon: 'success',
      title: mensaje,
      timer: 1500,
      showConfirmButton: false,
      background: '#1a1d21',
      color: '#ffffff'
    });
  }

}
