import { ComponentFixture, TestBed } from '@angular/core/testing';

import { EstadoResultado } from './estado-resultado';

describe('EstadoResultado', () => {
  let component: EstadoResultado;
  let fixture: ComponentFixture<EstadoResultado>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [EstadoResultado]
    })
    .compileComponents();

    fixture = TestBed.createComponent(EstadoResultado);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
