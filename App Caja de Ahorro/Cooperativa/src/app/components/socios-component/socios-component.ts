import { Component, Input, input, OnInit, signal } from '@angular/core';
import { SociosService } from '../../services/socios';

import { Coperativa } from '../../models/Coperativa';
import { Router } from '@angular/router';
import { Observable } from 'rxjs';
import { Socio } from '../../models/Socio';
import { CommonModule } from '@angular/common';
import jsPDF from 'jspdf';
import autoTable from 'jspdf-autotable';
import { forkJoin } from 'rxjs';
import * as XLSX from 'xlsx'; // Asegúrate de tener instalada la librería: npm install xlsx
import Swal from 'sweetalert2';

@Component({
  selector: 'app-socios-component',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './socios-component.html',
  styleUrl: './socios-component.css',
})

export class SociosComponent implements OnInit {

  // cooperativa!: Coperativa;
  cooperativa: Coperativa = { nombre: '', items: [] };
  client: Socio[] = [];
  listaSocios: any[] = [];

  constructor(private service: SociosService, private router: Router) { }

  irAAgregarSocio(): void {
    this.router.navigate(['/add-socio']);
  }

  irAAgregarAporte(): void {
    this.router.navigate(['/aportes']);
  }

  irAAgregarPagos(): void {
    this.router.navigate(['/pagos']);
  }

  irAAgregarPrestamos(): void {
    this.router.navigate(['/prestamos']);
  }

  irAAgregarCaja(): void {
    this.router.navigate(['/caja']);
  }

  // En tu componente (ej. SociosComponent)
  cargarSocios() {
    this.service.getSocios().subscribe({
      next: (data: Coperativa) => {
        // Accedemos a 'items' para que coincida con el tipo any[]
        this.listaSocios = data.items;
        console.log("Socios cargados:", this.listaSocios);
      },
      error: (err) => console.error("Error al cargar socios", err)
    });
  }

  // Tu función de mapeo ahora funcionará correctamente
  obtenerDatosCooperativa() {
    return this.listaSocios.map(socio => ({
      idsocio: socio.idsocio,
      nombresocio: socio.nombresocio,
      aportado: socio.aportado,
      montoprestado: socio.montoprestado,
      montopagado: socio.montopagado,
      montopendiente: socio.montopendiente,
      interesprestado: socio.interesprestado,
      interespagado: socio.interespagado,
      interesanulado: socio.interesanulado,
      interespendiente: socio.interespendiente,
    }));
  }

  // Esta función limpia los datos para que el Excel sea idéntico a tu BD
  obtenerDatosParaExportar() {
    const data = this.cooperativa.items;
    if (!data) return [];

    return data.map(socio => ({
      idsocio: socio.idsocio,
      nombresocio: socio.nombresocio,
      aportado: socio.aportado,
      montoprestado: socio.montoprestado,
      montopagado: socio.montopagado,
      montopendiente: socio.montopendiente,
      interesprestado: socio.interesprestado,
      interespagado: socio.interespagado,
      interesanulado: socio.interesanulado,
      interespendiente: socio.interespendiente
    }));
  }

  exportarListaSociosPDF() {
    // const data = this.obtenerDatosCooperativa(); // Asumo que devuelve el array que usas en el *ngFor
    const data = this.cooperativa.items; // Usamos el array que usas en el @for

    if (!data || data.length === 0) {
      this.cargarSocios();
      alert("Preparando datos... intente de nuevo");
      return;
    }

    // 1. CÁLCULO DE TOTALES (Para que coincidan con el pie de la tabla)
    const totales = data.reduce((acc, item) => {
      acc.aportado += Number(item.aportado || 0);
      acc.ayuda += Number(item.ayuda || 0);
      acc.montoprestado += Number(item.montoprestado || 0);
      acc.montopagado += Number(item.montopagado || 0);
      acc.montopendiente += Number(item.montopendiente || 0);
      acc.interesprestado += Number(item.interesprestado || 0);
      acc.interespagado += Number(item.interespagado || 0);
      acc.interesanulado += Number(item.interesanulado || 0);
      acc.interespendiente += Number(item.interespendiente || 0);
      acc.ingresos += Number(item.ingresos || 0);
      acc.gastos += Number(item.gastos || 0);
      acc.ganado += Number(item.ganado || 0);
      acc.total += Number(item.total || 0);
      return acc;
    }, {
      aportado: 0, ayuda: 0, montoprestado: 0, montopagado: 0, montopendiente: 0,
      interesprestado: 0, interespagado: 0, interesanulado: 0, interespendiente: 0,
      ingresos: 0, gastos: 0, ganado: 0, total: 0
    });

    const doc = new jsPDF('landscape');

    // Encabezado
    doc.setFontSize(16);
    doc.setTextColor(46, 204, 113);
    doc.text('Caja de Ahorro Familia Unida', doc.internal.pageSize.getWidth() / 2, 12, { align: 'center' });

    // 2. GENERACIÓN DE TABLA
    autoTable(doc, {
      startY: 20,
      margin: { left: 5, right: 5 },
      head: [[
        'ID', 'Nombre', '# Ap', 'Aportado', 'Ayuda', 'Préstamo', 'P. Pag', 'P. Pend',
        'Int. Tot', 'Int. Pag', 'Int. Anul', 'Int. Pend', 'Ingr.', 'Gasto', 'Ganado', 'TOTAL'
      ]],
      body: data.map((item: any) => [
        item.idsocio,
        item.nombresocio,
        item.num_aportes, // Según tu HTML: $any(item).num_aportes
        this.formatCur(item.aportado),
        this.formatCur(item.ayuda),
        this.formatCur(item.montoprestado),
        this.formatCur(item.montopagado),
        this.formatCur(item.montopendiente),
        this.formatCur(item.interesprestado),
        this.formatCur(item.interespagado),
        this.formatCur(item.interesanulado),
        this.formatCur(item.interespendiente),
        this.formatCur(item.ingresos),
        this.formatCur(item.gastos),
        this.formatCur(item.ganado),
        this.formatCur(item.total)
      ]),
      foot: [[
        '', 'TOTALES GENERALES', '',
        this.formatCur(totales.aportado),
        this.formatCur(totales.ayuda),
        this.formatCur(totales.montoprestado),
        this.formatCur(totales.montopagado),
        this.formatCur(totales.montopendiente),
        this.formatCur(totales.interesprestado),
        this.formatCur(totales.interespagado),
        this.formatCur(totales.interesanulado),
        this.formatCur(totales.interespendiente),
        this.formatCur(totales.ingresos),
        this.formatCur(totales.gastos),
        this.formatCur(totales.ganado),
        this.formatCur(totales.total)
      ]],
      theme: 'grid',
      headStyles: { fillColor: [46, 204, 113], fontSize: 6.5, halign: 'center' },
      footStyles: { fillColor: [46, 204, 113], fontSize: 6.5, halign: 'right' },
      styles: { fontSize: 6, cellPadding: 0.8 },
      columnStyles: {
        0: { cellWidth: 8 },  // ID
        1: { cellWidth: 32 }, // Nombre (más espacio)
        2: { cellWidth: 7 },  // # Aportes
      }
    });

    doc.save('Reporte_Caja_Familia_Unida.pdf');
  }

  // Función auxiliar para formatear moneda simple en el PDF
  private formatCur(value: any) {
    if (value === undefined || value === null) return "$0.00";
    return "$" + Number(value).toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
  }

  ngOnInit(): void {
    // this.cargarSocios(); // Carga los socios al iniciar el componente
    this.service.getSocios().subscribe(
      (data: any) => {
        // OPCIÓN A: Si quieres mantener la estructura de 'Coperativa'
        this.cooperativa = {
          nombre: "Caja de Ahorro Familia Unida",
          items: data // Aquí metemos el arreglo que viene del backend
        };
        console.log('Datos mapeados:', this.cooperativa);
      },
      (error) => {
        console.error('Error:', error);
      }
    );
    // this.service.getSocios().subscribe(res => {
    //   this.cooperativa = res; // Aquí se asigna el objeto que contiene 'nombre' e 'items'
    // });
    // this.service.getResumenCompleto().subscribe({
    //   next: (data) => {
    //     // Guardamos la respuesta en el objeto que usa el HTML
    //     this.cooperativa.items = data;
    //   },
    //   error: (err) => console.error('Error al cargar la lista interactiva', err)
    // });
  }

  // Función para sumar una propiedad de todos los socios en la lista
  getTotalGeneral(propiedad: string): number {
    if (!this.cooperativa.items) return 0;
    return this.cooperativa.items.reduce((acc, item) => {
      // Usamos 'keyof any' o 'keyof Socio' para indexar correctamente
      const valor = item[propiedad as keyof typeof item];
      return acc + (Number(valor) || 0);
    }, 0);
  }

  // 1. ELIMINAR
  // eliminarSocio(idSocio: any): void {
  //   const password = prompt('Ingrese la contraseña de administrador:');

  //   if (password === '1998') {
  //     const confirmar = confirm(`¿Está seguro de eliminar el socio #${idSocio}?`);

  //     if (confirmar) {
  //       this.service.eliminarSocio(idSocio).subscribe({
  //         next: (response) => {
  //           // CAMBIO AQUÍ: Filtramos la lista dentro de 'items'
  //           this.cooperativa.items = this.cooperativa.items.filter(s => s.idsocio !== idSocio);

  //           alert('Registro eliminado correctamente.');
  //         },
  //         error: (err) => {
  //           console.error('Error detectado:', err);
  //           const msg = err.error?.message || 'No se puede eliminar porque tiene pagos.';
  //           alert('ERROR: ' + msg);
  //         }
  //       });
  //     }
  //   } else if (password !== null) {
  //     alert('Contraseña incorrecta.');
  //   }
  // }

  eliminarSocio(idSocio: any): void {
    // 1. Usamos SweetAlert2 para la contraseña (evita bloqueos de Electron)
    Swal.fire({
      title: 'Eliminar Socio',
      text: 'Ingrese la contraseña de administrador:',
      input: 'password',
      inputAttributes: {
        autocapitalize: 'off'
      },
      showCancelButton: true,
      confirmButtonText: 'Validar',
      cancelButtonText: 'Cancelar',
      confirmButtonColor: '#d33', // Color rojo para advertir acción crítica
    }).then((result:any) => {
      if (result.isConfirmed) {
        if (result.value === '1998') {

          // 2. Segunda confirmación visual antes de borrar
          Swal.fire({
            title: `¿Está seguro de eliminar el socio #${idSocio}?`,
            text: "Esta acción no se puede deshacer.",
            icon: 'warning',
            showCancelButton: true,
            confirmButtonColor: '#3085d6',
            cancelButtonColor: '#d33',
            confirmButtonText: 'Sí, eliminar',
            cancelButtonText: 'Cancelar'
          }).then((confirmacion) => {
            if (confirmacion.isConfirmed) {
              this.ejecutarBajaSocio(idSocio);
            }
          });

        } else {
          Swal.fire('Error', 'Contraseña incorrecta.', 'error');
        }
      }
    });
  }

  // Lógica de comunicación con el backend (Docker)
  private ejecutarBajaSocio(idSocio: any) {
    this.service.eliminarSocio(idSocio).subscribe({
      next: (response) => {
        // 3. ACTUALIZACIÓN CORRECTA: Usamos 'items' según tu estructura
        if (this.cooperativa && this.cooperativa.items) {
          this.cooperativa.items = this.cooperativa.items.filter(s => s.idsocio !== idSocio);
        }

        Swal.fire('Eliminado', 'El socio ha sido eliminado correctamente.', 'success');
      },
      error: (err) => {
        console.error('Error detectado:', err);
        // Mensaje de error detallado (ej. si el socio tiene préstamos activos)
        const msg = err.error?.message || 'No se puede eliminar porque el socio tiene registros asociados.';
        Swal.fire('Error', msg, 'error');
      }
    });
  }

  // 2. ACTUALIZAR
  // editarSocio(socio: any) {
  //   if (socio && socio.idsocio) {
  //     // Usar la ruta completa con el ID
  //     this.router.navigate([`/editar-socio/${socio.idsocio}`])
  //       .then(nav => console.log('Navegación:', nav))
  //       .catch(err => console.error('Error:', err));
  //   }
  // }

  editarSocio(socio: any) {
    // 1. Verificación de seguridad con SweetAlert2
    Swal.fire({
      title: 'Seguridad de Administrador',
      text: `Ingrese la contraseña para editar al socio: ${socio.nombre}`,
      input: 'password',
      inputAttributes: {
        autocapitalize: 'off'
      },
      showCancelButton: true,
      confirmButtonText: 'Validar',
      cancelButtonText: 'Cancelar',
      confirmButtonColor: '#28a745'
    }).then((result:any) => {
      if (result.isConfirmed) {
        // 2. Validación de la clave maestra
        if (result.value === '1998') {

          if (socio && socio.idsocio) {
            console.log('Navegando a edición de socio ID:', socio.idsocio);

            // 3. Navegación usando la ruta dinámica
            this.router.navigate([`/editar-socio/${socio.idsocio}`])
              .then(nav => {
                if (!nav) {
                  console.error('La navegación falló. Verifica que la ruta /editar-socio/:id exista.');
                }
              })
              .catch(err => console.error('Error en el Router:', err));

          } else {
            Swal.fire('Error', 'No se pudo identificar el ID del socio.', 'error');
            console.error('Objeto socio no válido:', socio);
          }

        } else {
          // Alerta de contraseña incorrecta
          Swal.fire({
            icon: 'error',
            title: 'Acceso Denegado',
            text: 'La contraseña ingresada es incorrecta.',
            timer: 1500,
            showConfirmButton: false
          });
        }
      }
    });
  }

  prestamosDelSocio(idSocio: any) {
    this.router.navigate([`/prestamosdelsocio/${idSocio}`])
      .then(nav => console.log('Navegación:', nav))
      .catch(err => console.error('Error:', err));
  }

  pagosDelSocio(idSocio: any) {
    this.router.navigate([`/pagosdelsocio/${idSocio}`])
      .then(nav => console.log('Navegación:', nav))
      .catch(err => console.error('Error:', err));
  }

  botonDescargarPDF() {
    // Ahora 'listaPrestamos' ya existe y la función acepta 1 argumento
    this.exportarListaSociosPDF();
  }

  // ... dentro de tu clase SociosComponent

  // 1. GENERAR EXCEL (.xlsx)
  exportarSociosExcel() {
    const data = this.obtenerDatosParaExportar(); // Reutiliza el mapeo que ya arreglamos
    // const data = this.cooperativa.items; // Usamos el array que usas en el @for


    if (!data || data.length === 0) {
      this.cargarSocios();
      alert("Sincronizando datos... intente de nuevo.");
      return;
    }

    // 1. Creamos el libro y la hoja de trabajo
    const worksheet = XLSX.utils.json_to_sheet(data);
    const workbook = XLSX.utils.book_new();

    // 2. Insertamos los datos en una hoja llamada 'Aportes'
    XLSX.utils.book_append_sheet(workbook, worksheet, 'Aportes');

    // 3. Generamos el archivo y disparamos la descarga
    // El nombre incluirá los encabezados automáticamente
    XLSX.writeFile(workbook, `Socios_Familia_Unida_${new Date().toLocaleDateString()}.xlsx`);
  }

  // 2. GENERAR CSV (.csv)
  exportarSociosCSV() {
    const data = this.obtenerDatosParaExportar();
    // const data = this.cooperativa.items; // Usamos el array que usas en el @for

    if (!data || data.length === 0) {
      this.cargarSocios();
      alert("Los datos se están sincronizando. Por favor, intente de nuevo en un segundo.");
      return;
    }

    // 1. Extraemos los nombres de las columnas (idaporte, fechaaporte, etc.)
    const encabezados = Object.keys(data[0]).join(";");

    // 2. Mapeamos las filas de datos
    const filas = data.map(obj =>
      Object.values(obj).join(";")
    ).join("\n");

    // 3. Unimos encabezados y filas con un salto de línea
    const contenidoCompleto = `${encabezados}\n${filas}`;

    const blob = new Blob([contenidoCompleto], { type: 'text/csv;charset=utf-8;' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement("a");
    link.href = url;
    link.download = `importar_socios_postgres.csv`;
    link.click();
  }
}