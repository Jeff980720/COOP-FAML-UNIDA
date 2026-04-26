// import { Component, signal } from '@angular/core';
// import { RouterModule, RouterOutlet } from '@angular/router';

// @Component({
//   selector: 'app-root',
//   standalone: true,
//   // ¡Importante! Debes importar RouterModule (o al menos RouterOutlet y RouterLink)
//   // para que los enlaces y el outlet funcionen.
//   imports: [RouterOutlet, RouterModule],
//   templateUrl: './app.html',
//   styleUrl: './app.css'
// })
// export class App {
//   protected readonly title = signal('Cooperativa');
//   currentYear: number = new Date().getFullYear();
// }

import { Component, OnInit } from '@angular/core';
import { Router, NavigationEnd, RouterOutlet, RouterModule } from '@angular/router';
import { filter } from 'rxjs/operators';
import { CommonModule } from '@angular/common'; // <-- 1. Importa esto
import { SociosService } from './services/socios';
import { MatIconModule } from '@angular/material/icon'; // Importante para que funcione <mat-icon>
import { MatTableModule } from '@angular/material/table';

@Component({
  selector: 'app-root',
  standalone: true,
  // ¡Importante! Debes importar RouterModule (o al menos RouterOutlet y RouterLink)
  // para que los enlaces y el outlet funcionen.
  imports: [RouterOutlet, RouterModule, CommonModule, MatIconModule,MatTableModule],
  templateUrl: './app.html',
  styleUrl: './app.css'
})
export class App implements OnInit {

  nombreUsuario: string = '';
  mostrarMenu: boolean = false; // Por defecto oculto
  currentYear: number = new Date().getFullYear();

  constructor(
    private sociosService: SociosService,
    private router: Router
  ) {
    // Escuchar cambios de ruta para decidir si mostrar el menú
    this.router.events.pipe(
      filter(event => event instanceof NavigationEnd)
    ).subscribe(() => {
      this.validarAcceso();
    });
  }

  ngOnInit() {
    this.validarAcceso();
  }

  validarAcceso() {
    const token = localStorage.getItem('token');
    // Si hay token y no estamos en la página de login, mostramos el menú
    this.mostrarMenu = !!token && !this.router.url.includes('/login');

    if (this.mostrarMenu) {
      this.nombreUsuario = localStorage.getItem('nombre') || 'Usuario';
    }
  }

  // Cambia tu función salir por esta nueva:
  confirmarSalida() {
    const mensaje = '¿Está seguro que desea cerrar sesión en la Caja de Ahorro?';

    if (confirm(mensaje)) {
      this.salir();
    }
  }

  salir() {
    this.sociosService.logout();
    this.mostrarMenu = false;
    // Opcional: limpiar variables locales
    this.nombreUsuario = '';
  }
}