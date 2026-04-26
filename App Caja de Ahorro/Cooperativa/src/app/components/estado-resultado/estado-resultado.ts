import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { FormBuilder, FormsModule } from '@angular/forms';
import { ActivatedRoute, Router } from '@angular/router';
import { SociosService } from '../../services/socios';
import { forkJoin } from 'rxjs';
import * as XLSX from 'xlsx'; // Asegúrate de tener instalada la librería: npm install xlsx
import jsPDF from 'jspdf';
import autoTable from 'jspdf-autotable';

@Component({
  selector: 'app-estado-resultado',
  imports: [CommonModule, FormsModule],
  templateUrl: './estado-resultado.html',
  styleUrl: './estado-resultado.css',
})
export class EstadoResultado {
  // 1. Declara la variable para que TypeScript la reconozca
  listaAportes: any[] = [];
  listaPrestamos: any[] = [];
  listaPagos: any[] = [];
  listaCaja: any[] = [];

  constructor(public router: Router, private fb: FormBuilder, public service: SociosService, private route: ActivatedRoute) {

  }
  hoy = new Date().toISOString().split('T')[0];
  // Variables para los inputs de fecha
  // fechaInicioPeriodo: string = new Date().toISOString().split('T')[0];;
  // fechaInicioOperacion: string = new Date().toISOString().split('T')[0];;
  // fechaFinPeriodo: string = new Date().toISOString().split('T')[0];;
  fechaInicioPeriodo: string = "";
  fechaInicioOperacion: string = "";
  fechaFinPeriodo: string = "";

  ngOninit() {
    // Si ya hay fechas en el servicio, ejecutamos el reporte automáticamente al volver a la pestaña
    if (this.service.fechaInicioOperacion && this.service.fechaInicioPeriodo && this.service.fechaFinPeriodo) {
      this.actualizarReporte();
    }
    this.cargarAportes();
    this.cargarPrestamos();
    this.cargarPagos();
    this.cargarCaja();
  }

  // 2. Trae los datos de tu servicio
  cargarAportes() {
    this.service.getAportes().subscribe({
      next: (data) => {
        this.listaAportes = data;
        console.log("Datos listos para exportar:", this.listaAportes.length);
      },
      error: (err) => console.error("Error al cargar aportes:", err)
    });
  }

  cargarPrestamos() {
    this.service.getPrestamos().subscribe({
      next: (data) => {
        this.listaPrestamos = data;
        console.log("Datos listos para exportar:", this.listaPrestamos.length);
      },
      error: (err) => console.error("Error al cargar prestamos:", err)
    });
  }

  cargarPagos() {
    this.service.getPagos().subscribe({
      next: (data) => {
        this.listaPagos = data;
        console.log("Datos listos para exportar:", this.listaPagos.length);
      },
      error: (err) => console.error("Error al cargar pagos:", err)
    });
  }

  cargarCaja() {
    this.service.getCaja().subscribe({
      next: (data) => {
        this.listaCaja = data;
        console.log("Datos listos para exportar:", this.listaCaja.length);
      },
      error: (err) => console.error("Error al cargar caja:", err)
    });
  }

  // Tu función de mapeo ahora funcionará correctamente
  obtenerDatosAportes() {
    return this.listaAportes.map(aporte => ({
      // El orden debe coincidir con tu CREATE TABLE (image_bf0861)
      idaporte: aporte.idaporte,
      fechaaporte: this.formatearFechaISO(aporte.fechaaporte), // Formato YYYY-MM-DD
      aportado: Number(aporte.aportado).toFixed(2), // SOLO números, sin "$" (image_beb2c8)
      idsocio: aporte.idsocio,
      nombresocio: aporte.nombresocio,
      comentario: aporte.comentario || '' // Evitar "undefined" o "S/N" para que sea NULL o vacío
    }));
  }

  obtenerDatosPrestamos() {
    return this.listaPrestamos.map(prestamo => ({
      idprestamos: prestamo.idprestamos,
      tipo: prestamo.tipo,
      idsocio: prestamo.idsocio,
      nombresocio: prestamo.nombresocio,
      fechaprestamo: this.formatearFechaISO(prestamo.fechaprestamo),
      montoprestado: Number(prestamo.montoprestado).toFixed(2),
      plazoprestamo: Number(prestamo.plazoprestamo),
      interesprestamo: Number(prestamo.interesprestamo).toFixed(2),
      interesmensual: Number(prestamo.interesmensual).toFixed(2),
      interestotal: Number(prestamo.interestotal).toFixed(2),
      amortizacion: Number(prestamo.amortizacion).toFixed(2),
      cuota: Number(prestamo.cuota).toFixed(2),
      total: Number(prestamo.total).toFixed(2),
      comentario: prestamo.comentario || ''
    }))
    // El orden debe coincidir con tu CREATE TABLE
  }

  obtenerDatosPagos() {
    return this.listaPagos.map(pago => ({
      idpagos: pago.idpagos,
      tipo: pago.tipo,
      idsocio: pago.idsocio,
      nombresocio: pago.nombresocio,
      idprestamos: pago.idprestamos,
      amortizacion: pago.amortizacion,
      fechaamortizacion: this.formatearFechaISO(pago.fechaamortizacion),
      numamortizacion: pago.numamortizacion,
      interes: Number(pago.interes).toFixed(2),
      fechainteres: this.formatearFechaISO(pago.fechainteres),
      numinteres: pago.numinteres,
      comentario: pago.comentario || ''
    }))
    // El orden debe coincidir con tu CREATE TABLE
  }

  obtenerDatosCaja() {
    return this.listaCaja.map(caja => ({
      idcaja: caja.idcaja,
      efectivo: Number(caja.efectivo).toFixed(2),
      fechacaja: this.formatearFechaISO(caja.fechacaja),
      comentario: caja.comentario || ''
    }))
    // El orden debe coincidir con tu CREATE TABLE
  }

  // Función auxiliar para fechas compatibles con Postgres (YYYY-MM-DD)
  formatearFechaISO(fecha: any): string {
    const d = new Date(fecha);
    return d.toISOString().split('T')[0];
  }

  exportarAportesCSV() {
    const data = this.obtenerDatosAportes();

    if (!data || data.length === 0) {
      this.cargarAportes();
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
    link.download = `importar_aportes_postgres.csv`;
    link.click();
  }

  exportarPrestamosCSV() {
    const data = this.obtenerDatosPrestamos();

    if (!data || data.length === 0) {
      this.cargarPrestamos();
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
    link.download = `importar_prestamos_postgres.csv`;
    link.click();
  }

  exportarPagosCSV() {
    const data = this.obtenerDatosPagos();

    if (!data || data.length === 0) {
      this.cargarPagos();
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
    link.download = `importar_pagos_postgres.csv`;
    link.click();
  }

  exportarCajaCSV() {
    const data = this.obtenerDatosCaja();

    if (!data || data.length === 0) {
      this.cargarCaja();
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
    link.download = `importar_caja_postgres.csv`;
    link.click();
  }

  exportarAportesExcel() {
    const data = this.obtenerDatosAportes(); // Reutiliza el mapeo que ya arreglamos

    if (!data || data.length === 0) {
      this.cargarAportes();
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
    XLSX.writeFile(workbook, `Aportes_Familia_Unida_${new Date().toLocaleDateString()}.xlsx`);
  }

  exportarPrestamosExcel() {
    const data = this.obtenerDatosPrestamos(); // Reutiliza el mapeo que ya arreglamos

    if (!data || data.length === 0) {
      this.cargarPrestamos();
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
    XLSX.writeFile(workbook, `Aportes_Familia_Unida_${new Date().toLocaleDateString()}.xlsx`);
  }

  exportarPagosExcel() {
    const data = this.obtenerDatosPagos(); // Reutiliza el mapeo que ya arreglamos

    if (!data || data.length === 0) {
      this.cargarPagos();
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
    XLSX.writeFile(workbook, `Aportes_Familia_Unida_${new Date().toLocaleDateString()}.xlsx`);
  }

  exportarCajaExcel() {
    const data = this.obtenerDatosCaja(); // Reutiliza el mapeo que ya arreglamos

    if (!data || data.length === 0) {
      this.cargarCaja();
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
    XLSX.writeFile(workbook, `Aportes_Familia_Unida_${new Date().toLocaleDateString()}.xlsx`);
  }

  public exportarListaAportesPDF() {
    const data = this.obtenerDatosAportes();

    if (!data || data.length === 0) {
      this.cargarAportes();
      alert("Sincronizando datos... intente de nuevo.");
      return;
    }

    // Formato Vertical (Portrait)
    const doc = new jsPDF('p', 'mm', 'a4');
    const pageWidth = doc.internal.pageSize.getWidth();

    doc.setFontSize(18);
    doc.setTextColor(46, 204, 113); // Verde Cooperativa
    doc.text('Caja de Ahorro Familia Unida', pageWidth / 2, 15, { align: 'center' });

    doc.setFontSize(12);
    doc.setTextColor(100);
    doc.text('REGISTRO DE APORTES - LISTA GENERAL', pageWidth / 2, 22, { align: 'center' });

    autoTable(doc, {
      startY: 30,
      margin: { left: 10, right: 10 }, // Define márgenes laterales iguales
      head: [['##', 'Fecha Aporte', 'Monto', 'Cod. Client', 'Nombre Cliente', 'Observaciones']],
      body: data.map(p => [
        `A-${p.idaporte}`,
        new Date(p.fechaaporte).toLocaleDateString('es-ES'),
        `$${Number(p.aportado).toFixed(2)}`,
        p.idsocio,
        p.nombresocio.toUpperCase(),
        p.comentario || ''
      ]),
      theme: 'grid',
      headStyles: { fillColor: [46, 204, 113], halign: 'center', fontSize: 10 },
      styles: { fontSize: 8, cellPadding: 2, overflow: 'linebreak' },
      columnStyles: {
        0: { cellWidth: 15, halign: 'center' }, // ID más estrecho
        1: { cellWidth: 25, halign: 'center' }, // Fecha
        2: { cellWidth: 22, halign: 'right' },  // Monto alineado a la derecha
        3: { cellWidth: 22, halign: 'center' }, // Cod. Client
        4: { cellWidth: 45 },                   // Nombre Cliente
        5: { cellWidth: 'auto' }                // Observaciones toma el resto del margen
      }
    });

    const finalY = (doc as any).lastAutoTable.finalY + 10;
    const totalAportado = data.reduce((acc, p) => acc + Number(p.aportado), 0);

    doc.setFontSize(10);
    doc.setTextColor(0);
    doc.setFont('helvetica', 'bold');

    // Alineamos el total con el margen izquierdo
    doc.text(`Total Aportado: $${totalAportado.toFixed(2)}`, 10, finalY);

    doc.save(`Lista_Aportes_${new Date().toLocaleDateString()}.pdf`);
  }

  public exportarListaPrestamosPDF() {
    const data = this.obtenerDatosPrestamos();

    if (!data || data.length === 0) {
      this.cargarPrestamos();
      alert("Sincronizando datos... intente de nuevo.");
      return;
    }

    // --- CÁLCULO DE TOTALES FILTRADOS ---
    // Sumamos solo si el tipo NO es 'AYUDA'
    const carteraTotal = data
      .filter(p => p.tipo !== 'AYUDA')
      .reduce((acc, p) => acc + Number(p.montoprestado), 0);

    const interesAcumulado = data
      .filter(p => p.tipo !== 'AYUDA')
      .reduce((acc, p) => acc + Number(p.interestotal), 0);

    const doc = new jsPDF('landscape');
    const pageWidth = doc.internal.pageSize.getWidth();

    // Encabezado (Tu código existente)
    doc.setFontSize(18);
    doc.setTextColor(46, 204, 113);
    doc.text('Caja de Ahorro Familia Unida', pageWidth / 2, 15, { align: 'center' });

    doc.setFontSize(12);
    doc.setTextColor(100);
    doc.text('REGISTRO DE PRÉSTAMOS - LISTA GENERAL', pageWidth / 2, 22, { align: 'center' });

    // Tabla
    autoTable(doc, {
      startY: 30,
      head: [['##', 'Tipo', 'Cod. Client', 'Nombre Cliente', 'Monto', 'Fecha Prestamo', 'Plazo', 'Interes', 'Total Int', 'Cuota', 'Observaciones']],
      body: data.map(p => [
        `P-${p.idprestamos}`,
        p.tipo,
        p.idsocio,
        p.nombresocio.toUpperCase(),
        `$${p.montoprestado}`,
        new Date(p.fechaprestamo).toLocaleDateString('es-ES'),
        `${p.plazoprestamo}`,
        `${p.interesprestamo}%`,
        `$${p.interestotal}`,
        `$${p.cuota}`,
        p.comentario
      ]),
      theme: 'grid',
      headStyles: { fillColor: [46, 204, 113], halign: 'center', fontSize: 9 },
      styles: { fontSize: 8, cellPadding: 2 },
      columnStyles: { 0: { cellWidth: 15 }, 3: { cellWidth: 45 }, 10: { cellWidth: 50 } }
    });

    // --- PIE DE PÁGINA CON TOTALES ---
    const finalY = (doc as any).lastAutoTable.finalY + 10;
    doc.setFontSize(10);
    doc.setTextColor(0);

    // Fecha a la izquierda
    doc.text(`Fecha de Impresión: ${new Date().toLocaleString()}`, 14, finalY);

    // Totales a la derecha para que se vean ordenados
    doc.setFont('helvetica', 'bold');
    doc.text(`Cartera Total Prestada: $${carteraTotal.toFixed(2)}`, pageWidth - 15, finalY, { align: 'right' });
    doc.text(`Interés Total Proyectado: $${interesAcumulado.toFixed(2)}`, pageWidth - 15, finalY + 7, { align: 'right' });

    doc.save(`Lista_Prestamos_${new Date().toLocaleDateString()}.pdf`);
  }

  public exportarListaPagosPDF() {
    const data = this.obtenerDatosPagos();

    if (!data || data.length === 0) {
      this.cargarPagos();
      alert("Sincronizando datos... intente de nuevo.");
      return;
    }

    const carteraTotal = data
      .filter(p => p.tipo !== 'EGRESO' && p.idsocio !== '02')
      .reduce((acc, p) => acc + Number(p.amortizacion), 0);

    const interesAcumulado = data
      .filter(p => p.tipo !== 'EGRESO' && p.idsocio !== '02')
      .reduce((acc, p) => acc + Number(p.interes), 0);

    const doc = new jsPDF('landscape');
    const pageWidth = doc.internal.pageSize.getWidth();

    doc.setFontSize(18);
    doc.setTextColor(46, 204, 113);
    doc.text('Caja de Ahorro Familia Unida', pageWidth / 2, 15, { align: 'center' });

    doc.setFontSize(12);
    doc.setTextColor(100);
    doc.text('REGISTRO DE PAGOS - LISTA GENERAL', pageWidth / 2, 22, { align: 'center' });

    autoTable(doc, {
      startY: 30,
      head: [['##', 'Tipo', 'Cod.', 'Nombre Cliente', '# Pres', 'Amort.', 'F. Amort.', '# Am.', 'Interés', 'F. Int.', '# Int.', 'Observaciones']],
      body: data.map(p => [
        `P-${p.idpagos}`,
        p.tipo,
        p.idsocio,
        p.nombresocio.toUpperCase(),
        p.idprestamos || '',
        `$${Number(p.amortizacion).toFixed(2)}`,
        new Date(p.fechaamortizacion).toLocaleDateString('es-ES'),
        p.numamortizacion,
        `$${Number(p.interes).toFixed(2)}`,
        new Date(p.fechainteres).toLocaleDateString('es-ES'),
        p.numinteres,
        p.comentario
      ]),
      theme: 'grid',
      headStyles: { fillColor: [46, 204, 113], halign: 'center', fontSize: 7 }, // Reducimos tamaño fuente cabecera
      styles: { fontSize: 7, cellPadding: 1 }, // Reducimos padding y fuente general
      columnStyles: {
        0: { cellWidth: 12 }, // ID
        1: { cellWidth: 15 }, // Tipo
        2: { cellWidth: 10 }, // Cod. Socio
        3: { cellWidth: 40 }, // Nombre Cliente (más espacio)
        4: { cellWidth: 12 }, // # Prestamo
        5: { cellWidth: 18 }, // Amortización
        6: { cellWidth: 22 }, // Fecha Amort
        7: { cellWidth: 10 }, // # Amort
        8: { cellWidth: 18 }, // Interés
        9: { cellWidth: 22 }, // Fecha Int
        10: { cellWidth: 10 }, // # Int
        11: { cellWidth: 'auto' } // Observaciones ocupa el resto
      }
    });

    const finalY = (doc as any).lastAutoTable.finalY + 10;
    doc.setFontSize(10);
    doc.setTextColor(0);
    doc.text(`Cartera Total Recaudada: $${carteraTotal.toFixed(2)}`, 14, finalY);
    doc.text(`Interés Total Recaudado: $${interesAcumulado.toFixed(2)}`, 14, finalY + 7);

    doc.save(`Lista_Pagos_${new Date().toLocaleDateString()}.pdf`);
  }

  public exportarListaCajaPDF() {
    const data = this.obtenerDatosCaja();

    if (!data || data.length === 0) {
      this.cargarCaja();
      alert("Sincronizando datos... intente de nuevo.");
      return;
    }

    // Cambiamos a 'p' (portrait) o simplemente no pasamos parámetro (es el default)
    const doc = new jsPDF('p', 'mm', 'a4');
    const pageWidth = doc.internal.pageSize.getWidth();

    doc.setFontSize(18);
    doc.setTextColor(46, 204, 113);
    doc.text('Caja de Ahorro Familia Unida', pageWidth / 2, 15, { align: 'center' });

    doc.setFontSize(12);
    doc.setTextColor(100);
    doc.text('REGISTRO DE CIERRES DE CAJA - LISTA GENERAL', pageWidth / 2, 22, { align: 'center' });

    autoTable(doc, {
      startY: 30,
      head: [['##', 'Efectivo', 'Fecha Caja', 'Observaciones']],
      body: data.map(p => [
        `C-${p.idcaja}`, // Cambié P- por C- de "Caja" para diferenciarlo
        `$${Number(p.efectivo).toFixed(2)}`,
        new Date(p.fechacaja).toLocaleDateString('es-ES'),
        p.comentario
      ]),
      theme: 'grid',
      headStyles: { fillColor: [46, 204, 113], halign: 'center', fontSize: 10 },
      styles: { fontSize: 9, cellPadding: 3 }, // Aumentamos un poco el padding para que no se vea tan apretado
      columnStyles: {
        0: { cellWidth: 20 }, // ID
        1: { cellWidth: 35, halign: 'right' }, // Efectivo alineado a la derecha (estándar contable)
        2: { cellWidth: 35, halign: 'center' }, // Fecha Caja
        3: { cellWidth: 'auto' } // Observaciones ocupa todo el ancho restante
      }
    });

    const finalY = (doc as any).lastAutoTable.finalY + 10;

    // Añadimos un total de efectivo en caja al final del reporte
    const totalEfectivo = data.reduce((acc, p) => acc + Number(p.efectivo), 0);

    doc.setFontSize(10);
    doc.setTextColor(0);
    doc.setFont('helvetica', 'bold');

    doc.save(`Cierres_Caja_${new Date().toLocaleDateString()}.pdf`);
  }

  // Define las denominaciones siguiendo el orden del Excel
  billetes = [
    { denominacion: 20, fajos: 0, lomos: 0, saldos: 0, tipo: 'BILLETE' },
    { denominacion: 10, fajos: 0, lomos: 0, saldos: 0, tipo: 'BILLETE' },
    { denominacion: 5, fajos: 0, lomos: 0, saldos: 0, tipo: 'BILLETE' },
    { denominacion: 1, fajos: 0, lomos: 0, saldos: 0, tipo: 'BILLETE' }
  ];

  monedas = [
    { denominacion: 1.00, fajos: 0, lomos: 0, saldos: 0, tipo: 'MONEDA' },
    { denominacion: 0.50, fajos: 0, lomos: 0, saldos: 0, tipo: 'MONEDA' },
    { denominacion: 0.25, fajos: 0, lomos: 0, saldos: 0, tipo: 'MONEDA' },
    { denominacion: 0.10, fajos: 0, lomos: 0, saldos: 0, tipo: 'MONEDA' },
    { denominacion: 0.05, fajos: 0, lomos: 0, saldos: 0, tipo: 'MONEDA' },
    { denominacion: 0.01, fajos: 0, lomos: 0, saldos: 0, tipo: 'MONEDA' }
  ];

  // Función para calcular unidades totales por fila
  calcularUnidades(item: any): number {
    // Asegúrate de usar valores por defecto (0) para evitar errores si el input está vacío
    const f = item.fajos || 0;
    const l = item.lomos || 0;
    const s = item.saldos || 0;
    return (f * 100) + (l * 10) + s;
  }

  // Calcula el dinero total de una fila sumando fajos, lomos y saldos
  calcularTotalFila(item: any): number {
    const unidades = (item.fajos * 100) + (item.lomos * 10) + item.saldos;
    return unidades * item.denominacion;
  }

  // Getters para los totales generales
  get totalBilletes(): number {
    return this.billetes.reduce((acc, b) => acc + this.calcularTotalFila(b), 0);
  }

  get totalMonedas(): number {
    return this.monedas.reduce((acc, m) => acc + this.calcularTotalFila(m), 0);
  }

  // actualizarReporte() {
  //   // Verificamos que las fechas de operación y fin de periodo no estén vacías
  //   if (this.fechaInicioOperacion && this.fechaFinPeriodo) {
  //     // Llamamos al servicio pasando los strings directamente
  //     this.service.getResumenAportes(this.fechaInicioOperacion, this.fechaFinPeriodo).subscribe({
  //       next: (data) => {
  //         // Actualizamos el objeto que se muestra en el HTML
  //         this.estadoResultado.dineroAportado = data.dineroAportado;
  //       },
  //       error: (err) => console.error('Error al traer aportes:', err)
  //     });
  //   }
  // }

  // En tu archivo .ts
  actualizarReporte() {
    // 1. Verificamos si alguna fecha está vacía
    if (!this.service.fechaInicioOperacion || !this.service.fechaFinPeriodo || !this.service.fechaInicioPeriodo) {
      // Si están vacías, reseteamos todos los valores a 0
      this.service.estadoResultado.dineroAportado = 0;
      this.service.estadoResultado.dineroEnPrestamo = 0;
      this.service.estadoResultado.dineroAyuda = 0;
      this.service.estadoResultado.dineroPrestadoMensual = 0;
      this.service.estadoResultado.interesEnPrestamo = 0;
      this.service.estadoResultado.interesPrestadoMensual = 0;
      this.service.estadoResultado.dineroPaga = 0;
      this.service.estadoResultado.dineroPagadoMensual = 0;
      this.service.estadoResultado.interesPagado = 0;
      this.service.estadoResultado.interesPagadoMensual = 0;
      this.service.estadoResultado.dineroPorCobrar = 0;
      this.service.estadoResultado.interesPorCobrar = 0;
      this.service.estadoResultado.ingresosVarios = 0;
      this.service.estadoResultado.egresosSocios = 0;
      this.service.estadoResultado.egresoInteresAnulado = 0;
      this.service.estadoResultado.totalAportados = 0;
      this.service.estadoResultado.aporteIndividual = 0;
      this.service.estadoResultado.interesGeneradoReal = 0;
      this.service.estadoResultado.dineroEnCaja = 0;
      this.service.estadoResultado.dineroGenerado = 0;
      this.service.estadoResultado.verificacion = 0;
      // Haz esto para todas tus variables

      this.service.resultadoDelMes.dineroAportado = 0;
      this.service.resultadoDelMes.dineroPrestado = 0;
      this.service.resultadoDelMes.dineroPagadoEfectivo = 0;
      this.service.resultadoDelMes.dineroPagadoReprestamo = 0;
      this.service.resultadoDelMes.interesPrestado = 0;
      this.service.resultadoDelMes.interesPagado = 0;
      this.service.resultadoDelMes.dineroCajaAnterior = 0;
      this.service.resultadoDelMes.recaudado = 0;
      this.service.resultadoDelMes.prestado = 0;
      this.service.resultadoDelMes.dineroCajaActual = 0;
      // Haz esto para todas tus variables

      return; // Salimos de la función sin llamar al servicio
    }

    // Agrupamos todas las peticiones
    forkJoin({
      aportes: this.service.getResumenAportes(this.service.fechaInicioOperacion, this.service.fechaFinPeriodo),
      prestamos: this.service.getResumenPrestmos(this.service.fechaInicioOperacion, this.service.fechaFinPeriodo),
      ayuda: this.service.getResumenAyudas(this.service.fechaInicioOperacion, this.service.fechaFinPeriodo),
      prestamosMensuales: this.service.getResumenPrestmosMensual(this.service.fechaInicioPeriodo, this.service.fechaFinPeriodo),
      interesPrestamos: this.service.getResumenIntersPrestmos(this.service.fechaInicioOperacion, this.service.fechaFinPeriodo),
      interesPrestamoMensual: this.service.getResumenInteresPrestmosMensual(this.service.fechaInicioPeriodo, this.service.fechaFinPeriodo),
      pagos: this.service.getResumenDineroPagado(this.service.fechaInicioOperacion, this.service.fechaFinPeriodo),
      pagosMesuales: this.service.getResumenDineroPagadoMensual(this.service.fechaInicioPeriodo, this.service.fechaFinPeriodo),
      interesPagado: this.service.getResumenInteresPagado(this.service.fechaInicioOperacion, this.service.fechaFinPeriodo),
      interesPagadoMensual: this.service.getResumenInteresPagadoMensual(this.service.fechaInicioPeriodo, this.service.fechaFinPeriodo),
      ingresosVarios: this.service.getResumenIngresosVarios(this.service.fechaInicioOperacion, this.service.fechaFinPeriodo),
      gastosVarios: this.service.getResumenEgresosVarios(this.service.fechaInicioOperacion, this.service.fechaFinPeriodo),
      interesAnulado: this.service.getResumenInteresAnulado(this.service.fechaInicioOperacion, this.service.fechaFinPeriodo),
      totalAportes: this.service.getResumenAporteTotal(this.service.fechaInicioOperacion, this.service.fechaFinPeriodo),
      aporteindividual: this.service.getResumenAporteIndividual(this.service.fechaInicioOperacion, this.service.fechaFinPeriodo),
      dinerocaja: this.service.getResumenDineroCaja(this.service.fechaInicioPeriodo, this.service.fechaFinPeriodo),
      // Agrega aquí el resto de tus servicios...

      aporte2: this.service.getResumen1Aporte(this.service.fechaInicioPeriodo, this.service.fechaFinPeriodo),
      prestamo2: this.service.getResumen2Prestamo(this.service.fechaInicioPeriodo, this.service.fechaFinPeriodo),
      pagoEfectivo: this.service.getResumen3Efectivo(this.service.fechaInicioPeriodo, this.service.fechaFinPeriodo),
      pagoReprestamo: this.service.getResumen4Represtamo(this.service.fechaInicioPeriodo, this.service.fechaFinPeriodo),
      interesPrestado: this.service.getResumen5InteresPrestado(this.service.fechaInicioPeriodo, this.service.fechaFinPeriodo),
      interesPagado2: this.service.getResumen6InteresPagado(this.service.fechaInicioPeriodo, this.service.fechaFinPeriodo),
      cajaAnterior: this.service.getResumenCajaAnterior(this.service.fechaInicioPeriodo, this.service.fechaFinPeriodo),
      // Agrega aquí el resto de tus servicios...

    }).subscribe({
      next: (res) => {
        // 1. Asignamos valores
        this.service.estadoResultado.dineroAportado = res.aportes.dineroAportado || 0;
        this.service.estadoResultado.dineroEnPrestamo = res.prestamos.dineroEnPrestamo || 0;
        this.service.estadoResultado.dineroAyuda = res.ayuda.dineroAyuda || 0;
        this.service.estadoResultado.dineroPrestadoMensual = res.prestamosMensuales.dineroPrestadoMensual || 0;
        this.service.estadoResultado.interesEnPrestamo = res.interesPrestamos.interesEnPrestamo || 0;
        this.service.estadoResultado.interesPrestadoMensual = res.interesPrestamoMensual.interesPrestadoMensual || 0;
        this.service.estadoResultado.dineroPaga = res.pagos.dineroPaga || 0;
        this.service.estadoResultado.dineroPagadoMensual = res.pagosMesuales.dineroPagadoMensual || 0;
        this.service.estadoResultado.interesPagado = res.interesPagado.interesPagado || 0;
        this.service.estadoResultado.interesPagadoMensual = res.interesPagadoMensual.interesPagadoMensual || 0;
        this.service.estadoResultado.ingresosVarios = res.ingresosVarios.ingresosVarios || 0;
        this.service.estadoResultado.egresosSocios = res.gastosVarios.egresosSocios || 0;
        this.service.estadoResultado.egresoInteresAnulado = res.interesAnulado.egresoInteresAnulado || 0;
        this.service.estadoResultado.totalAportados = res.totalAportes.totalAportados || 0;
        this.service.estadoResultado.aporteIndividual = res.aporteindividual.aporteIndividual || 0;
        this.service.estadoResultado.dineroEnCaja = res.dinerocaja.dineroEnCaja || 0;

        this.service.resultadoDelMes.dineroAportado = res.aporte2.dineroAportado || 0;
        this.service.resultadoDelMes.dineroPrestado = res.prestamo2.dineroPrestado || 0;
        this.service.resultadoDelMes.dineroPagadoEfectivo = res.pagoEfectivo.dineroPagadoEfectivo || 0;
        this.service.resultadoDelMes.dineroPagadoReprestamo = res.pagoReprestamo.dineroPagadoReprestamo || 0;
        this.service.resultadoDelMes.interesPrestado = res.interesPrestado.interesPrestado || 0;
        this.service.resultadoDelMes.interesPagado = res.interesPagado2.interesPagado || 0;
        this.service.resultadoDelMes.dineroCajaAnterior = res.cajaAnterior.dineroCajaAnterior || 0;


        // 2. AHORA SÍ HACEMOS LAS RESTAS (Ya tenemos los datos)
        this.service.estadoResultado.dineroPorCobrar = this.service.estadoResultado.dineroEnPrestamo - this.service.estadoResultado.dineroPaga;
        this.service.estadoResultado.interesPorCobrar = this.service.estadoResultado.interesEnPrestamo - this.service.estadoResultado.interesPagado;
        this.service.estadoResultado.interesGeneradoReal = this.service.estadoResultado.interesEnPrestamo + this.service.estadoResultado.ingresosVarios - this.service.estadoResultado.egresosSocios - this.service.estadoResultado.egresoInteresAnulado;
        this.service.estadoResultado.dineroGenerado = this.service.estadoResultado.totalAportados + this.service.estadoResultado.interesGeneradoReal;
        this.service.estadoResultado.verificacion = this.service.estadoResultado.dineroPorCobrar + this.service.estadoResultado.interesPorCobrar + this.service.estadoResultado.dineroEnCaja;

        // this.resultadoDelMes.dineroCajaAnterior = this.estadoResultado.dineroEnCaja;
        this.service.resultadoDelMes.recaudado = this.service.resultadoDelMes.dineroAportado + this.service.resultadoDelMes.dineroPagadoEfectivo + this.service.resultadoDelMes.dineroPagadoReprestamo + this.service.resultadoDelMes.interesPagado + this.service.resultadoDelMes.dineroCajaAnterior;
        this.service.resultadoDelMes.prestado = this.service.resultadoDelMes.dineroPrestado;
        this.service.resultadoDelMes.dineroCajaActual = this.service.resultadoDelMes.recaudado - this.service.resultadoDelMes.prestado;

        this.calcularResultadosMensuales(); // <--- Llamada automática al recibir datos del servidor

      },
      error: (err) => console.error('Error en reporte:', err)
    });

  }

  // Función para recalcular los totales del recuadro "Caja de Ahorro"
  calcularResultadosMensuales() {
    // Aseguramos que los valores sean tratados como números para evitar concatenaciones de texto
    const aportado = Number(this.service.resultadoDelMes.dineroAportado) || 0;
    const efectivo = Number(this.service.resultadoDelMes.dineroPagadoEfectivo) || 0;
    const represtamo = Number(this.service.resultadoDelMes.dineroPagadoReprestamo) || 0;
    const interesPagado = Number(this.service.resultadoDelMes.interesPagado) || 0;
    const cajaAnterior = Number(this.service.resultadoDelMes.dineroCajaAnterior) || 0;
    const prestado = Number(this.service.resultadoDelMes.dineroPrestado) || 0;

    // Lógica de suma para RECAUDADO
    this.service.resultadoDelMes.recaudado = aportado + efectivo + represtamo + interesPagado + cajaAnterior;

    // Lógica para DINERO EN CAJA ACTUAL
    this.service.resultadoDelMes.prestado = prestado;
    this.service.resultadoDelMes.dineroCajaActual = this.service.resultadoDelMes.recaudado - this.service.resultadoDelMes.prestado;
  }

  resetearEstado() {
    this.service.fechaInicioOperacion = '';
    this.service.fechaInicioPeriodo = '';
    this.service.fechaFinPeriodo = '';
    this.service.estadoResultado.dineroAportado = 0;
    this.service.estadoResultado.dineroEnPrestamo = 0;
    this.service.estadoResultado.dineroAyuda = 0;
    this.service.estadoResultado.dineroPrestadoMensual = 0;
    this.service.estadoResultado.interesEnPrestamo = 0;
    this.service.estadoResultado.interesPrestadoMensual = 0;
    this.service.estadoResultado.dineroPaga = 0;
    this.service.estadoResultado.dineroPagadoMensual = 0;
    this.service.estadoResultado.interesPagado = 0;
    this.service.estadoResultado.interesPagadoMensual = 0;
    this.service.estadoResultado.dineroPorCobrar = 0;
    this.service.estadoResultado.interesPorCobrar = 0;
    this.service.estadoResultado.ingresosVarios = 0;
    this.service.estadoResultado.egresosSocios = 0;
    this.service.estadoResultado.egresoInteresAnulado = 0;
    this.service.estadoResultado.totalAportados = 0;
    this.service.estadoResultado.aporteIndividual = 0;
    this.service.estadoResultado.interesGeneradoReal = 0;
    this.service.estadoResultado.dineroEnCaja = 0;
    this.service.estadoResultado.dineroGenerado = 0;
    this.service.estadoResultado.verificacion = 0;
    // Haz esto para todas tus variables

    this.service.resultadoDelMes.dineroAportado = 0;
    this.service.resultadoDelMes.dineroPrestado = 0;
    this.service.resultadoDelMes.dineroPagadoEfectivo = 0;
    this.service.resultadoDelMes.dineroPagadoReprestamo = 0;
    this.service.resultadoDelMes.interesPrestado = 0;
    this.service.resultadoDelMes.interesPagado = 0;
    this.service.resultadoDelMes.dineroCajaAnterior = 0;
    this.service.resultadoDelMes.recaudado = 0;
    this.service.resultadoDelMes.prestado = 0;
    this.service.resultadoDelMes.dineroCajaActual = 0;
  }

  guardarCajaActual() {
    // Supongamos que esta es la variable donde tienes el total del arqueo
    const montoAEnviar = this.service.resultadoDelMes.dineroCajaActual;

    this.router.navigate(['/caja'], {
      queryParams: { monto: montoAEnviar }
    })
      .then(nav => console.log('Navegación con monto exitosa'))
      .catch(err => console.error('Error en navegación:', err));
  }

  obtenerDatosAportesParaExportar() {
    // Mapeamos tu lista de aportes (asumiendo que se llama listaAportes)
    return this.listaAportes.map(aporte => ({
      'Número de Aporte': aporte.idAporte,
      'Socio': aporte.nombreSocio,
      'Fecha': aporte.fechaAporte,
      'Monto Aportado': `$${Number(aporte.monto).toFixed(2)}`,
      'Comentario': aporte.comentario || 'S/N'
    }));
  }
}